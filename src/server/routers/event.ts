import { router, publicProcedure } from '../trpc';
import { TRPCError } from '@trpc/server';
import {
  EventFormSchema,
  switchUpdateSchema,
  getClosingSoon,
  getEventSchema,
  getFeatured,
  getEventsByIdSchema,
  getEventCustomers,
} from '~/schema/event';
import { prisma } from '~/server/prisma';
import { verifyJWT } from '~/utils/jwt';

export const eventRouter = router({
  get: publicProcedure.input(getEventSchema).query(async ({ input }) => {
    try {
      const { filters } = input;
      const filterPayload: any = { ...filters };

      delete filterPayload.searchQuery;
      delete filterPayload.endDate;
      delete filterPayload.startDate;
      delete filterPayload.status;

      const where: any = {
        is_deleted: false,
        lang_id: input.lang_id,
        // draw_date: null,
        // is_winnar_selected: !true,
        ...filterPayload,


      };
      if (input?.filters?.searchQuery) {
        where.OR = [];
        where.OR.push({
          name: {
            contains: input?.filters?.searchQuery,
            mode: 'insensitive',
          },
        });
      }

      const today = new Date();
      if (input?.filters?.status == 'active') {
        where.launch_date = { lte: today };
        where.end_date = { gte: today };
      }
      if (input?.filters?.status == 'closed') {
        where.end_date = { lte: today };
      }
      if (input?.filters?.startDate) {
        const startDate = new Date(input?.filters?.startDate)
          ?.toISOString()
          .split('T')[0] as string;
        where.launch_date = { gte: new Date(startDate) };
      }
      if (input?.filters?.endDate) {
        const endDate = new Date(input?.filters?.endDate)
          ?.toISOString()
          .split('T')[0] as string;
        where.end_date = { lte: new Date(endDate) };
      }

      if (input.category_id) where.category_id = input.category_id;

      const totalEventPromise = prisma.eventView.count({
        where: where,
      });

      const eventPromise = prisma.eventView.findMany({
        orderBy: { created_at: 'desc' },
        skip: input.first * input.rows,
        take: input.rows,
        where: where,
      });

      const [totalEvent, event] = await Promise.all([
        totalEventPromise,
        eventPromise,
      ]);

      if (!event?.length) {
        throw new TRPCError({
          code: 'NOT_FOUND',
          message: 'Events not found',
        });
      }

      return {
        message: 'events found',
        count: totalEvent,
        data: event,
      };
    } catch (error: any) {
      throw new TRPCError({
        code: 'INTERNAL_SERVER_ERROR',
        message: error?.message,
      });
    }
  }),
  create: publicProcedure.input(EventFormSchema).mutation(async ({ input }) => {
    try {
      const { en, ar, multi_image, meta, ...eventPayload } = input;

      const createPayload = {
        ...eventPayload,
        charity_id: 1,
        user_id: 1,
        // is_winnar_selected: true,
        meta: undefined as string | undefined,
      };

      if (meta) {
        createPayload.meta = JSON.stringify(meta);
      } else {
        delete createPayload.meta;
      }

      const eventDescPayload = [
        { ...en, lang_id: 1 },
        { ...ar, lang_id: 2 },
      ];
      const myImages = multi_image.map((str: string) => ({
        thumb: str,
      }));

      const event = await prisma.event.create({
        data: {
          ...createPayload,

          EventDescription: { createMany: { data: eventDescPayload } },
          EventImages: { createMany: { data: myImages } },
        },
      });

      if (!event) {
        throw new TRPCError({
          code: 'BAD_REQUEST',
          message: 'Event not created',
        });
      }

      const ticketsPayload = [...Array(event.total_tickets)].map(
        (_, index) => ({
          event_id: event.id,
          ticket_num: index + 1,
        }),
      );

      prisma.eventTickets
        .createMany({ data: ticketsPayload })
        .then((res) => console.log(res, 'eventTickets created'))
        .catch((err) => console.log(err, 'eventTickets rejected'))
        .finally(() => console.log('resolve done!'));

      return { data: event, message: 'Event created' };
    } catch (error: any) {
      throw new TRPCError({
        code: 'INTERNAL_SERVER_ERROR',
        message: error?.message,
      });
    }
  }),
  update: publicProcedure.input(EventFormSchema).mutation(async ({ input }) => {
    try {
      const {
        en,
        ar,
        multi_image,
        removed_images,
        meta,
        event_id = 0,
        ...eventPayload
      } = input;

      const payload = {
        ...eventPayload,
        charity_id: 1,
        user_id: 1,
        // is_winnar_selected: true,
        meta: undefined as string | undefined,
      };

      if (meta) {
        payload.meta = JSON.stringify(meta);
      } else {
        delete payload.meta;
      }

      if (!payload.faq_id) payload.faq_id = undefined;

      const event = await prisma.event.update({
        where: { id: event_id },
        data: payload,
      });

      const myImages = multi_image.map((str: string) => ({
        thumb: str,
        event_id,
      }));

      const createEventImages = prisma.eventImage.createMany({
        data: myImages,
      });

      const eventEnPromise = prisma.eventDescription.updateMany({
        where: { event_id, lang_id: 1 },
        data: en,
      });

      const eventArPromise = prisma.eventDescription.updateMany({
        where: { event_id, lang_id: 2 },
        data: ar,
      });

      await Promise.all([eventEnPromise, eventArPromise, createEventImages]);
      if (removed_images?.length) {
        await prisma.eventImage.deleteMany({
          where: { id: { in: removed_images } },
        });
      }

      if (!event) {
        throw new TRPCError({
          code: 'BAD_REQUEST',
          message: 'Event not updated',
        });
      }

      const eventTicketCounts = await prisma.eventTickets.count({
        where: { event_id: event.id },
      });

      // case 1- Adding tickets
      if (eventTicketCounts > 0 && event.total_tickets > eventTicketCounts) {
        const ticketMargin = event.total_tickets - eventTicketCounts;

        const ticketsPayload = [...Array(ticketMargin)].map((_, index) => ({
          event_id: event.id,
          ticket_num: index + 1 + eventTicketCounts,
        }));

        prisma.eventTickets
          .createMany({ data: ticketsPayload })
          .then((res) => console.log(res, 'eventTickets resolve'))
          .catch((err) => console.log(err, 'eventTickets error'))
          .finally(() => console.log('resolve done!'));

        // case 2- Removing tickets
      } else if (
        eventTicketCounts > 0 &&
        event.total_tickets < eventTicketCounts
      ) {
        const assignedEventTicketCounts = await prisma.eventTickets.count({
          where: { event_id: event.id, customer_id: { not: null } },
        });

        if (event.total_tickets >= assignedEventTicketCounts) {
          prisma.eventTickets
            .deleteMany({
              where: {
                event_id: event.id,
                ticket_num: {
                  gt: event.total_tickets,
                  lte: eventTicketCounts,
                },
                customer_id: null,
              },
            })
            .then((res) => console.log(res, 'eventTickets resolve'))
            .catch((err) => console.log(err, 'eventTickets error'))
            .finally(() => console.log('resolve done!'));
        } else {
          await prisma.event.update({
            where: { id: event.id },
            data: { total_tickets: eventTicketCounts },
          });

          throw new TRPCError({
            code: 'FORBIDDEN',
            message: "Tickets are assigned, can't remove tickets!",
          });
        }
      }

      return { data: event, message: 'Event updated' };
    } catch (error: any) {
      throw new TRPCError({
        code: 'INTERNAL_SERVER_ERROR',
        message: error?.message,
      });
    }
  }),
  getEventCustomers: publicProcedure
    .input(getEventCustomers)
    .query(async ({ input }) => {
      try {
        const eventCustomers =
          await prisma.$queryRaw`SELECT e.id AS event_id, e.thumb,e.end_date, e.price, oe.customer_id, c.email,c.phone_number,ed.name AS event_name, c.first_name, c.last_name, 
          CAST( SUM( oe.quantity ) AS INT ) AS quantity,
          CAST( SUM( oe.quantity * oe.ticket_price ) AS INT ) AS sub_total_amount,
          CAST( SUM( o.discount_amount ) AS INT ) AS discount_amount,
          CAST( SUM( ( oe.quantity * oe.ticket_price ) - discount_amount ) AS INT ) AS total_amount
            FROM event AS e
            JOIN event_description AS ed
            ON e.id = ed.event_id
            JOIN order_event AS oe
            ON e.id = oe.event_id
            JOIN public."order" AS o
            ON o.id = oe.order_id
            JOIN customer AS c
            ON c.id = oe.customer_id
            GROUP BY e.id, c.id,oe.customer_id,ed.id
            HAVING e.id = ${input.event_id} AND ed.lang_id= 1
            order BY quantity DESC  
          `;

        return {
          message: 'events found',
          data: eventCustomers,
        };
      } catch (error: any) {
        throw new TRPCError({
          code: 'INTERNAL_SERVER_ERROR',
          message: error?.message,
        });
      }
    }),

  switchUpdate: publicProcedure
    .input(switchUpdateSchema)
    .mutation(async ({ input }) => {
      try {
        const event = await prisma.event.update({
          where: { id: input.id },
          data: { [input.type]: input.value },
        });
        if (!event) {
          throw new TRPCError({
            code: 'BAD_REQUEST',
            message: 'Event not found',
          });
        }

        return { data: event, message: 'Event updated' };
      } catch (error: any) {
        throw new TRPCError({
          code: 'INTERNAL_SERVER_ERROR',
          message: error?.message,
        });
      }
    }),

  getByCategoryId: publicProcedure
    .input(getEventSchema)
    .query(async ({ input }) => {
      try {
        const today = new Date();
        const where: any = {
          is_deleted: false,
          is_enabled: true,
          launch_date: { lte: today },
          end_date: { gte: today },
          // draw_date: null,
        };

        if (input?.filters?.startDate && !input?.filters?.endDate) {
          const startDate = new Date(input?.filters?.startDate);
          where.created_at = { gte: startDate };
        }
        if (input?.filters?.endDate && !input?.filters?.startDate) {
          const endDate = new Date(input?.filters?.endDate);
          where.created_at = { lte: endDate };
        }
        if (input?.filters?.endDate && input?.filters?.startDate) {
          const startDate = new Date(input?.filters?.startDate);
          const endDate = new Date(input?.filters?.endDate);

          where.created_at = { gte: startDate, lte: endDate };
        }

        if (input.category_id) where.category_id = input.category_id;

        const totalEventPromise = prisma.event.count({
          where: where,
        });

        const eventPromise = prisma.event.findMany({
          orderBy: { created_at: 'desc' },
          skip: input.first * input.rows,
          take: input.rows,
          where: where,
          include: {
            EventDescription: {
              where: {
                lang_id: input?.lang_id,
              },
              select: {
                comp_details: true,
                lang_id: true,
                name: true,
                desc: true,
              },
            },
          },
        });

        const [totalEvent, event] = await Promise.all([
          totalEventPromise,
          eventPromise,
        ]);

        if (!event?.length) {
          throw new TRPCError({
            code: 'NOT_FOUND',
            message: 'Events not found',
          });
        }

        return {
          message: 'Events found',
          count: totalEvent,
          data: event,
        };
      } catch (error: any) {
        throw new TRPCError({
          code: 'INTERNAL_SERVER_ERROR',
          message: error?.message,
        });
      }
    }),

  getUpcoming: publicProcedure
    .input(getClosingSoon)
    .query(async ({ input }) => {
      try {
        const where: any = {
          is_deleted: false,
          is_enabled: true,
          // draw_date: null,
        };
        const todayDate = new Date();
        const endingDate = new Date();
        endingDate.setDate(endingDate.getDate() + 90);

        // upcoming means its going to start
        if (input?.type == 'upcoming') where.launch_date = { gte: todayDate };
        if (input?.type == 'closing') {
          where.launch_date = { lte: new Date(todayDate) };
          where.end_date = {
            gte: new Date(todayDate),
            lte: new Date(endingDate),
          };
        }
        if (input?.type == 'drawn'){
          where.draw_date = { lte: new Date(todayDate) };
          // where.draw_date = { not: null };
        } 
        if (input?.category_id) where.category_id = input?.category_id;

        const totalEventPromise = prisma.event.count({
          where: where,
        });

        const eventPromise = prisma.event.findMany({
          orderBy: { created_at: 'desc' },
          skip: input.first * input.rows,
          take: input.rows,
          where: where,
          include: {
            EventDescription: {
              where: {
                lang_id: input?.lang_id,
              },
              select: {
                id: true,
                lang_id: true,
                desc: true,
                name: true,
                comp_details: true,
              },
            },
          },
        });

        const [totalEvent, event] = await Promise.all([
          totalEventPromise,
          eventPromise,
        ]);

        if (!event?.length) {
          throw new TRPCError({
            code: 'NOT_FOUND',
            message: 'Events not found',
          });
        }

        return {
          message: 'Events found',
          count: totalEvent,
          data: event,
        };
      } catch (error: any) {
        throw new TRPCError({
          code: 'INTERNAL_SERVER_ERROR',
          message: error?.message,
        });
      }
    }),

  getFeatured: publicProcedure.input(getFeatured).query(async ({ input }) => {
    try {
      const where: any = {
        is_deleted: false,
        is_enabled: true,
        end_date: { gte: new Date() },
        // draw_date: null,
        category_id: 1,
        EventDescription: { some: { lang_id: input?.lang_id } },
      };

      // upcoming means its going to start
      if (input?.is_featured == 1) where.is_featured = true;

      const totalEventPromise = prisma.event.count({
        where: where,
      });

      const eventPromise = prisma.event.findMany({
        orderBy: { created_at: 'desc' },
        skip: input.first * input.rows,
        take: input.rows,
        where: where,
        include: {
          EventImages: {},
          EventDescription: {
            where: {
              lang_id: input?.lang_id,
            },
            select: {
              id: true,
              lang_id: true,
              desc: true,
              name: true,
              comp_details: true,
            },
          },
        },
      });

      const [totalEvent, event] = await Promise.all([
        totalEventPromise,
        eventPromise,
      ]);

      if (!event?.length) {
        throw new TRPCError({
          code: 'NOT_FOUND',
          message: 'Events not found',
        });
      }

      return {
        message: 'Events found',
        count: totalEvent,
        data: event,
      };
    } catch (error: any) {
      throw new TRPCError({
        code: 'INTERNAL_SERVER_ERROR',
        message: error?.message,
      });
    }
  }),

  getEventsById: publicProcedure
    .input(getEventsByIdSchema)
    .query(async ({ input, ctx }) => {
      try {
        const descriptionPayload: any =
          input.type === 'admin' ? undefined : { lang_id: input.lang_id };

        const event = await prisma.event.findUnique({
          where: {
            id: input.id,
          },
          include: {
            Winner: {
              select: {
                ticket_num: true,
                Customer: {
                  select: {
                    first_name: true,
                    last_name: true,
                  },
                },
              },
            },
            EventDescription: {
              where: descriptionPayload,
              select: {
                id: true,
                lang_id: true,
                name: true,
                desc: true,
                comp_details: true,
              },
            },
            EventImages: true,
            CMS: {
              include: {
                CMSDescription: {
                  where: { lang_id: input.lang_id, is_deleted: false },
                  select: {
                    content: true,
                    lang_id: true,
                  },
                },
              },
            },
          },
        });

        const token = ctx?.req?.cookies['winnar-token'];
        let ticketPurchased = 0;

        let userData;
        if (token) {
          userData = await verifyJWT(token);

          const customerLimit = await prisma.orderEvent.groupBy({
            where: { event_id: input.id, customer_id: userData?.id },
            by: ['event_id', 'customer_id'],
            _sum: { quantity: true },
          });

          ticketPurchased = customerLimit[0]?._sum?.quantity ?? 0;
        }

        if (input.type === 'admin') {
          const assignedEventTicketCounts = await prisma.eventTickets.count({
            where: { event_id: input.id, customer_id: { not: null } },
          });

          return {
            message: 'events found',
            data: event,
            assignedEventTicketCounts,
          };
        } else {
          return {
            message: 'events found',
            data: event,
            ticketPurchased: ticketPurchased,
          };
        }
      } catch (error: any) {
        throw new TRPCError({
          code: 'INTERNAL_SERVER_ERROR',
          message: error?.message,
        });
      }
    }),
});
