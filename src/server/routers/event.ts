import { router, publicProcedure } from '../trpc';
import { TRPCError } from '@trpc/server';
import {
  EventFormSchema,
  createEventSchema,
  deleteEventSchema,
  getEventSchema,
} from '~/schema/event';
import { prisma } from '~/server/prisma';

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
  create: publicProcedure.input(EventFormSchema).mutation(async ({ input }) => {
    try {
      const { en, ar, multi_image, ...eventPayload } = input;
      const createPayload: any = {
        is_alt: eventPayload?.is_alt,
        thumb: eventPayload?.thumb,
        creator_id: 1,
        video_src: eventPayload?.video_src,
        category_id: +eventPayload?.category_id,
        price: +eventPayload?.price,
        total_tickets: +eventPayload?.total_tickets,
        user_ticket_limit: +eventPayload?.user_ticket_limit,
        cash_alt: +eventPayload?.cash_alt,
        launch_date: eventPayload?.launch_date,
        end_date: eventPayload?.end_date,
      };
      const event = await prisma.event.create({
        data: {
          ...createPayload,
        },
      });

      if (!event) {
        throw new TRPCError({
          code: 'BAD_REQUEST',
          message: 'Event not created',
        });
      }

      const eventDescPayload = [
        { ...en, event_id: event.id, lang_id: 1 },
        { ...ar, event_id: event.id, lang_id: 2 },
      ];

      const eventDesc = await prisma.eventDescription.createMany({
        data: eventDescPayload,
      });

      if (!eventDesc.count) {
        throw new TRPCError({
          code: 'BAD_REQUEST',
          message: 'Event Description not created',
        });
      }
      const myImages = multi_image.map((str: string, index: number) => ({
        thumb: str,
        event_id: event.id,
      }));
      const eventImages = await prisma.eventImage.createMany({
        data: myImages,
      });

      if (!eventImages.count) {
        throw new TRPCError({
          code: 'BAD_REQUEST',
          message: 'Event Images not created',
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
});
