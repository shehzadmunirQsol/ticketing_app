import { router, publicProcedure } from '../trpc';
import { TRPCError } from '@trpc/server';
import { cmsSchema, getCmsSchema, getCmsContentByIdSchema } from '~/schema/cms';
import { prisma } from '~/server/prisma';
import { hashPass, isSamePass } from '~/utils/hash';
import { serialize } from 'cookie';
import { signJWT, verifyJWT } from '~/utils/jwt';

export const cmsRouter = router({
  addCmsContent: publicProcedure
    .input(cmsSchema)
    .mutation(async ({ input }) => {
      try {
        console.log('INPUT :: ', input);

        const payload: any = {
          user_id: 1,
          slug: input.en.slug,
        };

        const cms = await prisma?.cMS?.create({
          data: payload,
        });
        console.log(cms, 'user');

        const descriptionPayload: any = {
          cms_id: cms?.id,
          title: input.en.title,
          desc: input.en.desc,
          meta_keywords: input.en.metadesc,
          lang_id: 1,
          content: input?.en.content,
        };
        const cmsDescription = await prisma?.cMSDescription?.create({
          data: descriptionPayload,
        });
        console.log(cmsDescription, 'cmsDescription user');
        return { status: 'success' };
      } catch (error: any) {
        throw new TRPCError({
          code: 'INTERNAL_SERVER_ERROR',
          message: error.message,
        });
      }
    }),

  getCmsContent: publicProcedure.input(getCmsSchema).query(async (input) => {
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

  getById: publicProcedure
    .input(getCmsContentByIdSchema)
    .query(async ({ input }) => {
      try {
        const cms = await prisma?.cMS?.findUnique({
          where: { id: input.id },
          include: {
            CMSDescription: true,
          },
        });
        if (!cms) {
          throw new TRPCError({
            code: 'NOT_FOUND',
            message: 'Page not found',
          });
        }

        return { message: 'Cms Page found', data: cms };
      } catch (error: any) {
        throw new TRPCError({
          code: 'INTERNAL_SERVER_ERROR',
          message: error?.message,
        });
      }
    }),
});
