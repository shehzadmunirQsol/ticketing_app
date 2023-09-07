import { router, publicProcedure } from '../trpc';
import { TRPCError } from '@trpc/server';
import { prisma } from '~/server/prisma';
import { getWinnersByIdSchema } from '~/schema/winners';
import { hashPass, isSamePass } from '~/utils/hash';
import { signJWT } from '~/utils/jwt';
import { serialize } from 'cookie';
import { generateOTP, sendEmail } from '~/utils/helper';

export const winnerRouter = router({
  getWinnersById: publicProcedure
    .input(getWinnersByIdSchema)
    .query(async ({ ctx, input }) => {
      try {
        const winnersPromise = await prisma.winner.findMany({
          skip: input.first * input.rows,
          take: input.rows,
          include: {
            Customer: true,
            Event: {
              select: {
                EventDescription: {
                  where: {
                    lang_id: 1,
                  },
                  select: {
                    id: true,
                    lang_id: true,
                    name: true,
                    desc: true,
                    comp_details: true,
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


        console.log(totalWinners, winners,"totalWinners, winners")

        // console.log(winners, 'WINNERS');
      //  le.log(totalEventPromise, 'totalEventPromise');

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
});
