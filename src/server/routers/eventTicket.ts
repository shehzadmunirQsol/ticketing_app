import { router, publicProcedure } from '../trpc';
import { TRPCError } from '@trpc/server';
import { getEventTicketsSchema } from '~/schema/eventTicket';
import { prisma } from '~/server/prisma';

export const eventTicketRouter = router({
  get: publicProcedure.input(getEventTicketsSchema).query(async ({ input }) => {
    try {
      const eventTickets = await prisma.eventTickets.findMany({
        select: {
          ticket_num: true,
        },
        where: {
          event_id: input.event_id,
          order_event_id: input.order_event_id,
        },
      });

      if (!eventTickets?.length) {
        throw new TRPCError({
          code: 'NOT_FOUND',
          message: 'Tickets not found',
        });
      }

      return { message: 'Tickets found', data: eventTickets };
    } catch (error: any) {
      throw new TRPCError({
        code: 'INTERNAL_SERVER_ERROR',
        message: error?.message,
      });
    }
  }),
});
