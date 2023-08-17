import { router, publicProcedure } from '../trpc';
import { TRPCError } from '@trpc/server';
import {
  createEventSchema,
  deleteEventSchema,
  getEventSchema,
} from '~/schema/event';
import { prisma } from '~/server/prisma';

export const eventRouter = router({
  get: publicProcedure.input(getEventSchema).query(async ({ input }) => {
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
  create: publicProcedure
    .input(createEventSchema)
    .mutation(async ({ input }) => {
      try {
        const { en, ar, ...eventPayload } = input;

        const event = await prisma.event.create({ data: eventPayload });

        if (!event) {
          throw new TRPCError({
            code: 'BAD_REQUEST',
            message: 'Event not created',
          });
        }

        const eventDescPayload = [
          { ...en, event_id: event.id },
          { ...ar, event_id: event.id },
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
