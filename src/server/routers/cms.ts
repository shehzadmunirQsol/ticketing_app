import { router, publicProcedure } from '../trpc';
import { TRPCError } from '@trpc/server';
import { cmsSchema, getCmsSchema } from '~/schema/cms';
import { prisma } from '~/server/prisma';
import { hashPass, isSamePass } from '~/utils/hash';
import { serialize } from 'cookie';
import { signJWT, verifyJWT } from '~/utils/jwt';

export const cmsRouter = router({
  cmsAboutUs: publicProcedure.input(cmsSchema).mutation(async ({ input }) => {
    try {
      console.log('INPUT :: ', input);
      const payload: any = {
        user_id: 1,
        slug: 'sadasdasdasd.asdsad/sad/sad/sa/d/sad/sad',
      };

      const cms = await prisma?.cMS?.create({
        data: payload,
      });
      console.log(cms, 'user');

      const descriptionPayload: any = {
        cms_id: cms?.id,
        title: 'title',
        desc: 'pending',
        meta_keywords: 'hello',
        lang_id: 1,
        content: input?.detail,
      };
      const cmsDescription = await prisma?.cMSDescription?.create({
        data: descriptionPayload,
      });
      console.log(cmsDescription, 'cmsDescription user');
      return { status: 'success', cms, cmsDescription };
    } catch (error: any) {
      throw new TRPCError({
        code: 'INTERNAL_SERVER_ERROR',
        message: error.message,
      });
    }
  }),

  getCmsAboutUs: publicProcedure.input(getCmsSchema).query(async (input) => {
    try {
      const cms = await prisma?.cMS?.findMany({
        include: {
          CMSDescription: true,
        },
      });
      console.log(cms, 'cms');

      return cms;
    } catch (error: any) {
      throw new TRPCError({
        code: 'INTERNAL_SERVER_ERROR',
        message: error.message,
      });
    }
  }),
});
