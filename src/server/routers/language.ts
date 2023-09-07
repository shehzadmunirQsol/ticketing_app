import { router, publicProcedure } from '../trpc';
import { TRPCError } from '@trpc/server';
import { prisma } from '~/server/prisma';

export const languageRouter = router({
  get: publicProcedure.query(async () => {
    try {
      const languages = await prisma.language.findMany();

      if (!languages?.length) {
        throw new TRPCError({
          code: 'NOT_FOUND',
          message: 'Languages not found',
        });
      }

      return { message: 'Languages found', data: languages };
    } catch (error: any) {
      throw new TRPCError({
        code: 'INTERNAL_SERVER_ERROR',
        message: error?.message,
      });
    }
  }),
});
