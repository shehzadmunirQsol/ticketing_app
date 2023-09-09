import { router, publicProcedure } from '../trpc';
import { TRPCError } from '@trpc/server';
import {
  EventFormSchema,
  deleteEventSchema,
  getClosingSoon,
  getEventSchema,
  getFeatured,
  getEventsByIdSchema,
} from '~/schema/event';
import { prisma } from '~/server/prisma';
import { verifyJWT } from '~/utils/jwt';

export const eventRouter = router({
  get: publicProcedure.input(getEventSchema).query(async ({ input }) => {
    try {
      const where: any = { is_deleted: false, lang_id: input.lang_id };

      if (input?.startDate) {
        const startDate = new Date(input?.startDate);
        where.created_at = { gte: startDate };
      }
      if (input?.endDate) {
        const endDate = new Date(input?.endDate);
        where.created_at = { lte: endDate };
      }
      if (input.category_id) where.id = input.category_id;

      // if (input.event_id) where.id = input.event_id;

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
      const { en, ar, multi_image, ...eventPayload } = input;
      const createPayload = {
        ...eventPayload,
        charity_id: 1,
        user_id: 1,
      };
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
        event_id = 0,
        ...eventPayload
      } = input;
      const payload = {
        ...eventPayload,
        charity_id: 1,
        user_id: 1,
      };

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
          message: 'Event not created',
        });
      }

      return { data: event, message: 'Event created' };
    } catch (error: any) {
      throw new TRPCError({
        code: 'INTERNAL_SERVER_ERROR',
        message: error?.message,
      });
    }
  }),
  delete: publicProcedure
    .input(deleteEventSchema)
    .mutation(async ({ input }) => {
      try {
        const event = await prisma.event.update({
          where: { id: input.id },
          data: { is_deleted: true },
        });
        if (!event) {
          throw new TRPCError({
            code: 'BAD_REQUEST',
            message: 'Event not found',
          });
        }

        return { data: event, message: 'Event deleted' };
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
        console.log({ input }, 'event input ');
        const where: any = { is_deleted: false };

        if (input?.startDate) {
          const startDate = new Date(input?.startDate);
          where.created_at = { gte: startDate };
        }
        if (input?.endDate) {
          const endDate = new Date(input?.endDate);
          where.created_at = { lte: endDate };
        }

        if (input.category_id) where.category_id = input.category_id;

        const totalEventPromise = prisma.event.count({
          where: where,
        });

        const eventPromise = prisma.event.findMany({
          orderBy: { created_at: 'asc' },
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

        console.log(totalEvent, event, 'event data');
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

  getUpcomimg: publicProcedure
    .input(getClosingSoon)
    .query(async ({ input }) => {
      try {
        const where: any = {
          is_deleted: false,
          EventDescription: { some: { lang_id: input?.lang_id } },
        };
        const todayDate = new Date();
        const endingDate = new Date();
        endingDate.setDate(endingDate.getDate() + 21);

        // upcoming means its going to start
        if (input?.type == 'upcomming') where.launch_date = { gte: todayDate };
        if (input?.type == 'closing') {
          where.launch_date = { lte: todayDate };
          where.end_date = { gte: todayDate, lte: endingDate };
        }
        const totalEventPromise = prisma.event.count({
          where: where,
        });

        const eventPromise = prisma.event.findMany({
          orderBy: { created_at: 'asc' },
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

        console.log(totalEvent, event, 'event data');
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

  getClosingSoon: publicProcedure
    .input(getClosingSoon)
    .query(async ({ input }) => {
      try {
        const where: any = { is_deleted: false };

        if (input?.startDate) {
          const startDate = new Date(input?.startDate);
          where.created_at = { gte: startDate };
        }
        if (input?.endDate) {
          const endDate = new Date(input?.endDate);
          where.created_at = { lte: endDate };
        }

        if (input.event_id) where.id = input.event_id;

        const totalEventPromise = prisma.event.count({
          where: where,
        });

        const eventPromise = prisma.event.findMany({
          orderBy: { created_at: 'desc' },
          skip: input.first,
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

  getFeatured: publicProcedure.input(getFeatured).query(async ({ input }) => {
    try {
      const where: any = {
        is_deleted: false,
        EventDescription: { some: { lang_id: input?.lang_id } },
      };

      // upcoming means its going to start
      if (input?.is_featured == 1) where.is_featured = true;

      const totalEventPromise = prisma.event.count({
        where: where,
      });

      const eventPromise = prisma.event.findMany({
        orderBy: { created_at: 'asc' },
        skip: input.first * input.rows,
        take: input.rows,
        where: where,
        include: {
          EventImages: {},
          EventDescription: {},
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

      console.log(totalEvent, event, 'event data');
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
        const descriptionPayload :any =
          input.type === 'admin' ? undefined : { lang_id: input.lang_id };
        const event = await prisma.event.findUnique({
          where: {
            id: input.id,
          },
          include: {
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

        return {
          message: 'events found',
          data: event,
          ticketPurchased: ticketPurchased,
        };
      } catch (error: any) {
        throw new TRPCError({
          code: 'INTERNAL_SERVER_ERROR',
          message: error?.message,
        });
      }
    }),
});
