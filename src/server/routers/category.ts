import { router, publicProcedure } from '../trpc';
import { TRPCError } from '@trpc/server';
import { createCategorySchema } from '~/schema/category';
import { prisma } from '~/server/prisma';

export const categoryRouter = router({
  create: publicProcedure
    .input(createCategorySchema)
    .mutation(async ({ input, ctx }) => {
      try {
        const { thumb, creator_id, ...restPayload } = input;

        const category = await prisma.category.create({
          data: { thumb, creator_id },
        });
        if (!category) {
          throw new TRPCError({
            code: 'BAD_REQUEST',
            message: 'Category not created',
          });
        }
        const categoryDescPayload = {
          category_id: category.id,
          ...restPayload,
        };

        const categoryDesc = await prisma.categoryDescription.create({
          data: categoryDescPayload,
        });

        if (!categoryDesc) {
          throw new TRPCError({
            code: 'BAD_REQUEST',
            message: 'Category Description not created',
          });
        }

        return { data: category, message: 'Category created' };
      } catch (error: any) {
        console.log('data error', error);

        throw new TRPCError({
          code: 'INTERNAL_SERVER_ERROR',
          message: error?.message,
        });
      }
    }),
  update: publicProcedure
    .input(createCategorySchema)
    .mutation(async ({ input, ctx }) => {
      try {
        const { thumb, creator_id, ...restPayload } = input;

        const category = await prisma.category.create({
          data: { thumb, creator_id },
        });
        if (!category) {
          throw new TRPCError({
            code: 'BAD_REQUEST',
            message: 'Category not created',
          });
        }
        const categoryDescPayload = {
          category_id: category.id,
          ...restPayload,
        };

        const categoryDesc = await prisma.categoryDescription.create({
          data: categoryDescPayload,
        });

        if (!categoryDesc) {
          throw new TRPCError({
            code: 'BAD_REQUEST',
            message: 'Category Description not created',
          });
        }

        return { data: category, message: 'Category created' };
      } catch (error: any) {
        console.log('data error', error);

        throw new TRPCError({
          code: 'INTERNAL_SERVER_ERROR',
          message: error?.message,
        });
      }
    }),
  
});
