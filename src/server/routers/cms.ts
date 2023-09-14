import { router, publicProcedure } from '../trpc';
import { TRPCError } from '@trpc/server';
import {
  cmsSchema,
  getCmsSchema,
  getCmsContentByIdSchema,
  updateCmsContentById,
} from '~/schema/cms';
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
          slug: input.slug,
          type: input.type,
        };

        const cms = await prisma?.cMS?.create({
          data: payload,
        });

        const descriptionPayload: any = {
          cms_id: cms?.id,
          title: input.en.title,
          desc: input.en.desc,
          meta_keywords: input.en.metadesc,
          lang_id: 1,
          content: input?.content,
        };
        const cmsDescription = await prisma?.cMSDescription?.create({
          data: descriptionPayload,
        });
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

  updateById: publicProcedure
    .input(updateCmsContentById)
    .mutation(async ({ input }) => {
      try {
        console.log(input, 'HSJSJSJSHJ ::');
        const cms: any = await prisma?.cMS?.findUnique({
          where: { id: input.id },
          include: {
            CMSDescription: true,
          },
        });
        console.log(cms, 'cmscmscmscmscmscmscms');
        if (!cms) {
          throw new TRPCError({
            code: 'NOT_FOUND',
            message: 'Page not found',
          });
        }

        const cmsPayload = {
          slug: input.slug,
        };
        const cmsDescriptionPayload: any = {
          cms_id: cms?.id,
          title: input.en.title,
          desc: input.en.desc,
          meta_keywords: input.en.metadesc,
          lang_id: 1,
          content: input?.content,
        };

        const cmsUpdate: any = await prisma.cMS.update({
          where: {
            id: input.id,
          },
          data: cmsPayload,
        });
        const cmsDescriptionUpdate: any = await prisma.cMSDescription.update({
          where: {
            id: cms?.CMSDescription[0].id,
          },
          data: cmsDescriptionPayload,
        });

        return { message: 'Cms updating Successfully' };
      } catch (error: any) {
        throw new TRPCError({
          code: 'INTERNAL_SERVER_ERROR',
          message: error?.message,
        });
      }
    }),
});
