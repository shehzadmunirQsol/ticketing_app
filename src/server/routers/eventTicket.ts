import { router, publicProcedure } from '../trpc';
import { TRPCError } from '@trpc/server';
import {
  getAllEventTicketCustomersSchema,
  getEventTicketCustomerSchema,
  getEventTicketsSchema,
} from '~/schema/eventTicket';
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
  getEventTicketCustomer: publicProcedure
    .input(getEventTicketCustomerSchema)
    .query(async ({ input }) => {
      try {
        const eventTicket = await prisma.eventTickets.findFirst({
          select: {
            ticket_num: true,
            Customer: {
              select: {
                id: true,
                first_name: true,
                email: true,
              },
            },
          },
          where: {
            event_id: input.event_id,
            ticket_num: input?.ticket_num,
          },
        });

        if (!eventTicket) {
          throw new TRPCError({
            code: 'NOT_FOUND',
            message: 'Ticket not found',
          });
        }

        return { message: 'Ticket found', data: eventTicket };
      } catch (error: any) {
        throw new TRPCError({
          code: 'INTERNAL_SERVER_ERROR',
          message: error?.message,
        });
      }
    }),
  getAllEventTicketCustomer: publicProcedure
    .input(getAllEventTicketCustomersSchema)
    .mutation(async ({ input }) => {
      try {
        const eventTickets = await prisma.eventTickets.findMany({
          orderBy: {
            ticket_num: 'asc',
          },
          select: {
            ticket_num: true,
            updated_at: true,
            Customer: {
              select: {
                id: true,
                first_name: true,
              },
            },
          },
          where: {
            event_id: input.event_id,
            customer_id: { not: null },
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
