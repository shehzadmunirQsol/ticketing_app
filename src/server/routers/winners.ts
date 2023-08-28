import { router, publicProcedure } from '../trpc';
import { TRPCError } from '@trpc/server';
import { prisma } from '~/server/prisma';
import { getWinnersByIdSchema } from '~/schema/winners.ts';
import { hashPass, isSamePass } from '~/utils/hash';
import { signJWT } from '~/utils/jwt';
import { serialize } from 'cookie';
import { generateOTP, sendEmail } from '~/utils/helper';

export const winnerRouter = router({
  getWinnersById: publicProcedure
    .input(getWinnersByIdSchema)
    .query(async ({ ctx, input }) => {
      try {
        const winners = await prisma.winner.findMany({
          where: { customer_id: 2 },
          include: {
            Customer: true,
          },
          
        });
        console.log(winners,"WINNERS")

        return winners;
      } catch (error: any) {
        console.log({ error });
        throw new TRPCError({
          code: 'INTERNAL_SERVER_ERROR',
          message: error.message,
        });
      }
    }),
});
