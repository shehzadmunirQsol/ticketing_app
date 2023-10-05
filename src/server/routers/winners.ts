import { router, publicProcedure } from '../trpc';
import { TRPCError } from '@trpc/server';
import { prisma } from '~/server/prisma';
import { getWinnersSchema, selectWinnerSchema } from '~/schema/winners';
import { EMAIL_TEMPLATE_IDS, sendEmail } from '~/utils/helper';

export const winnerRouter = router({
  get: publicProcedure.input(getWinnersSchema).query(async ({ input }) => {
    try {
      const { filters, ...payload } = input;
      const filterPayload: any = { ...filters };

      if (filterPayload?.searchQuery) delete filterPayload.searchQuery;
      if (filterPayload?.endDate) delete filterPayload.endDate;
      if (filterPayload?.startDate) delete filterPayload.startDate;
      const where: any = { is_deleted: false, ...filterPayload };
      console.log({ filters }, 'filters_input');
      if (input?.filters?.searchQuery) {
        where.OR = [];
        where.OR.push({
          Event: {
            EventDescription: {
              some: {
                name: {
                  contains: input?.filters?.searchQuery,
                  mode: 'insensitive',
                },
              },
            },
          },
        });
        if (+input?.filters?.searchQuery) {
          where.OR.push({
            ticket_num: +input?.filters?.searchQuery,
          });
        }
        where.OR.push({
          Customer: {
            first_name: {
              contains: input?.filters?.searchQuery,
              mode: 'insensitive',
            },
          },
        });
        where.OR.push({
          Customer: {
            last_name: {
              contains: input?.filters?.searchQuery,
              mode: 'insensitive',
            },
          },
        });
        where.OR.push({
          Customer: {
            email: {
              contains: input?.filters?.searchQuery,
              mode: 'insensitive',
            },
          },
        });
      }

      if (input?.filters?.startDate && !input?.filters?.endDate) {
        const startDate = new Date(input?.filters?.startDate)
          ?.toISOString()
          .split('T')[0] as string;
        where.created_at = { gte: new Date(startDate) };
      }
      if (input?.filters?.endDate && !input?.filters?.startDate) {
        const endDate = new Date(input?.filters?.endDate)
          ?.toISOString()
          .split('T')[0] as string;
        where.created_at = { lte: new Date(endDate) };
      }
      if (input?.filters?.endDate && input?.filters?.startDate) {
        const startDate = new Date(input?.filters?.startDate)
          ?.toISOString()
          .split('T')[0] as string;
        const endDate = new Date(input?.filters?.endDate)
          ?.toISOString()
          .split('T')[0] as string;
        where.created_at = { gte: new Date(startDate), lte: new Date(endDate) };
      }

      const winnersPromise = prisma.winner.findMany({
        skip: input.first * input.rows,
        take: input.rows,
        orderBy: { created_at: 'desc' },
        where: where,
        select: {
          draw_date: true,
          is_cash_alt: true,
          ticket_num: true,
          Customer: {
            select: {
              id: true,
              first_name: true,
              email: true,
              phone_number: true,
            },
          },
          Event: {
            select: {
              id: true,
              thumb: true,
              EventDescription: {
                where: { lang_id: input.lang_id },
                select: {
                  name: true,
                },
              },
            },
          },
        },
      });

      const totalWinnersPromise = prisma.winner.count({
        where: {
          is_deleted: false,
        },
      });

      const [totalWinners, winners] = await Promise.all([
        totalWinnersPromise,
        winnersPromise,
      ]);

      return {
        message: 'Events found',
        count: totalWinners,
        data: winners,
      };
    } catch (error: any) {
      console.log({ error });
      throw new TRPCError({
        code: 'INTERNAL_SERVER_ERROR',
        message: error.message,
      });
    }
  }),

  selectWinner: publicProcedure
    .input(selectWinnerSchema)
    .mutation(async ({ input }) => {
      try {
        const { customer_email, customer_name, event_name, ...payloadIds } =
          input;

        const drawDate = new Date();
        const winnerPayload = {
          ...payloadIds,
          draw_date: drawDate,
          ticket_num: Math.floor(Math.random() * 99999),
          is_enabled: true,
        };

        const eventPromise = prisma.event.update({
          where: { id: input.event_id },
          data: { draw_date: drawDate },
        });
        const winnerPromise = prisma.winner.create({
          data: winnerPayload,
        });
        const deleteCartEvent = prisma.cartItem.updateMany({
          where: {
            event_id: input.event_id,
          },
          data: {
            is_deleted: true,
          },
        });

        await Promise.all([eventPromise, winnerPromise, deleteCartEvent]);

        const mailOptions = {
          template_id: EMAIL_TEMPLATE_IDS.SELECT_WINNER,
          from: 'no-reply@winnar.com',
          to: customer_email,
          subject: 'Winner Selected!',
          params: {
            first_name: customer_name,
            event_name: event_name,
          },
        };

        await sendEmail(mailOptions);

        return { message: 'Winner selected successfully!' };
      } catch (error: any) {
        console.log({ error });
        throw new TRPCError({
          code: 'INTERNAL_SERVER_ERROR',
          message: error.message,
        });
      }
    }),
});
