import { router, publicProcedure } from '../trpc';
import { TRPCError } from '@trpc/server';
import { prisma } from '~/server/prisma';
import {
  getWinnersSchema,
  selectWinnerSchema,
  updateWinnerSchema,
} from '~/schema/winners';
import { EMAILS, EMAIL_TEMPLATE_IDS, sendEmail } from '~/utils/helper';

export const winnerRouter = router({
  get: publicProcedure.input(getWinnersSchema).query(async ({ input }) => {
    try {
      const { filters, ...payload } = input;
      const filterPayload: any = { ...filters };

      delete filterPayload.searchQuery;
      if (filterPayload?.endDate) delete filterPayload.endDate;
      if (filterPayload?.startDate) delete filterPayload.startDate;
      const where: any = { is_deleted: false, ...filterPayload };

      if (input?.filters?.searchQuery?.trim()) {
        const query =
          isNaN(Number(input?.filters?.searchQuery?.trim())) === false
            ? Number(input?.filters?.searchQuery?.trim()).toLocaleString()
            : input?.filters?.searchQuery?.trim();
        console.log(
          query,
          input?.filters?.searchQuery,
          'input?.filters?.searchQuery',
        );
        where.OR = [];
        where.OR.push({
          Event: {
            EventDescription: {
              some: {
                name: {
                  contains: query,
                  mode: 'insensitive',
                },
              },
            },
          },
        });
        if (query) {
          where.OR.push({
            ticket_num: {
              contains: input?.filters?.searchQuery?.trim(),
              mode: 'insensitive',
            },
          });
        }
        where.OR.push({
          Customer: {
            first_name: {
              contains: query,
              mode: 'insensitive',
            },
          },
        });

        if (input.is_admin) {
          where.OR.push({
            Customer: {
              email: {
                contains: query,
                mode: 'insensitive',
              },
            },
          });
        }
      }
      if (input?.filters?.startDate && !input?.filters?.endDate) {
        const startDate = new Date(input?.filters?.startDate)
          ?.toISOString()
          .split('T')[0] as string;
        where.created_at = { gte: new Date(startDate) };
      }
      if (input?.filters?.endDate && !input?.filters?.startDate) {
        const inputEndDate = new Date(input?.filters?.endDate);
        const endDate = new Date(inputEndDate.setHours(23, 59));
        where.created_at = { lte: endDate };
      }
      if (input?.filters?.endDate && input?.filters?.startDate) {
        const startDate = new Date(input?.filters?.startDate)
          ?.toISOString()
          .split('T')[0] as string;
        const inputEndDate = new Date(input?.filters?.endDate);
        const endDate = new Date(inputEndDate.setHours(23, 59));
        where.created_at = { gte: new Date(startDate), lte: endDate };
      }
      if (input.is_admin === false) {
        where.is_enabled = true;
      }

      const winnersPromise = prisma.winner.findMany({
        skip: input.first * input.rows,
        take: input.rows,
        orderBy: { id: 'desc' },
        where: where,
        select: {
          id: true,
          thumb: true,
          is_enabled: true,
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
        where: where,
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

  update: publicProcedure
    .input(updateWinnerSchema)
    .mutation(async ({ input }) => {
      const { winner_id, ...payload } = input;
      const winner = await prisma.winner.update({
        where: { id: winner_id },
        data: payload,
      });

      return { data: winner, message: 'Winnar Updated successfully!' };
    }),
  selectWinner: publicProcedure
    .input(selectWinnerSchema)
    .mutation(async ({ input }) => {
      try {
        const { customer_email, customer_name, event_name, draw_date, ...payloadIds } = input;

        const drawDate = new Date();
        const winnerPayload = {
          ...payloadIds,
          draw_date: draw_date,
          ticket_num: input?.ticket_num?.toString(),
          is_enabled: false,
        };

        // const eventPromise = prisma.event.update({
        //   where: { id: input.event_id },
        //   data: { is_winnar_selected: true },
        // });
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

        // await Promise.all([eventPromise, winnerPromise, deleteCartEvent]);
        await Promise.all([winnerPromise, deleteCartEvent]);

        const mailOptions = {
          template_id: EMAIL_TEMPLATE_IDS.SELECT_WINNER,
          from: EMAILS.contact,
          to: customer_email,
          subject: 'Winnar Selected!',
          params: {
            first_name: customer_name,
            event_name: event_name,
          },
        };

        await sendEmail(mailOptions);

        return { message: 'Winnar selected successfully!' };
      } catch (error: any) {
        console.log({ error });
        throw new TRPCError({
          code: 'INTERNAL_SERVER_ERROR',
          message: error.message,
        });
      }
    }),
});
