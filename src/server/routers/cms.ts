import { router, publicProcedure } from '../trpc';
import { TRPCError } from '@trpc/server';
import {
  cmsSchema,
  getCmsSchema,
  getCmsContentByIdSchema,
  updateCmsContentById,
  cmsStatusUpdateById,
} from '~/schema/cms';
import { prisma } from '~/server/prisma';

export const cmsRouter = router({
  addCmsContent: publicProcedure
    .input(cmsSchema)
    .mutation(async ({ input }) => {
      try {
        const { en, ar, ...eventPayload } = input;
        const eventDescPayload = [
          { ...en, lang_id: 1 },
          { ...ar, lang_id: 2 },
        ];
        const payload: any = {
          user_id: 1,
          ...eventPayload,
        };

        await prisma?.cMS?.create({
          data: {
            ...payload,
            CMSDescription: { createMany: { data: eventDescPayload } },
          },
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
        where:{
          is_deleted:false
        },
        orderBy: { created_at: 'desc' },
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
        const cms: any = await prisma?.cMS?.findUnique({
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
        const { en, ar, ...eventPayload } = input;

        const payload: any = {
          user_id: 1,
          ...eventPayload,
        };

        await prisma.cMS.update({
          where: {
            id: input.id,
          },
          data: { ...payload },
        });
        const eventEnPromise = prisma.cMSDescription.updateMany({
          where: { cms_id: cms?.id, lang_id: 1 },
          data: en,
        });

        const eventArPromise = prisma.cMSDescription.updateMany({
          where: { cms_id: cms?.id, lang_id: 2 },
          data: ar,
        });
        await Promise.all([eventEnPromise, eventArPromise]);

        return { message: 'Cms updating Successfully' };
      } catch (error: any) {
        throw new TRPCError({
          code: 'INTERNAL_SERVER_ERROR',
          message: error?.message,
        });
      }
    }),

  cmsStatusUpdateById: publicProcedure
    .input(cmsStatusUpdateById)
    .mutation(async ({ input }) => {
      try {
        const cms: any = await prisma?.cMS?.findUnique({
          where: { id: input.id },
        });

        if (!cms) {
          throw new TRPCError({
            code: 'NOT_FOUND',
            message: 'Page not found',
          });
        }

        const { id, ...payload } = input;
         await prisma.cMS.update({
          where: {id: id,},
          data: payload,
        });

        return { message: 'CMS Status Updated Successfully' };
      } catch (error: any) {
        throw new TRPCError({
          code: 'INTERNAL_SERVER_ERROR',
          message: error?.message,
        });
      }
    }),
});
