import { router, publicProcedure } from '../trpc';
import { TRPCError } from '@trpc/server';
import {
  createCategorySchema,
  deleteCategorySchema,
  getCategorySchema,
} from '~/schema/category';
import { prisma } from '~/server/prisma';

export const categoryRouter = router({
  get: publicProcedure.input(getCategorySchema).query(async ({ input }) => {
    try {
      const where: any = { is_deleted: false };

      if (input?.startDate) {
        const startDate = new Date(input?.startDate);
        where.created_at = { gte: startDate };
      }
      if (input?.endDate) {
        const endDate = new Date(input?.endDate);
        where.created_at = { lte: endDate };
      }

      if (input.category_id) where.id = input.category_id;

      const totalCategoryPromise = prisma.category.count({
        where: where,
      });

      const categoryPromise = prisma.category.findMany({
        orderBy: { created_at: 'desc' },
        skip: input.first,
        take: input.rows,
        where: where,
        select: {
          id: true,
          thumb: true,
          created_at: true,
          updated_at: true,
          _count: true,
          CategoryDescription: {
            where: { lang_id: input.lang_id },

            select: {
              name: true,
              desc: true,
            },
          },
        },
      });

      const [totalCategory, category] = await Promise.all([
        totalCategoryPromise,
        categoryPromise,
      ]);

      if (!category?.length) {
        throw new TRPCError({
          code: 'NOT_FOUND',
          message: 'Categories not found',
        });
      }

      return {
        message: 'categories found',
        count: totalCategory,
        data: category,
      };
    } catch (error: any) {
      throw new TRPCError({
        code: 'INTERNAL_SERVER_ERROR',
        message: error?.message,
      });
    }
  }),
  create: publicProcedure
    .input(createCategorySchema)
    .mutation(async ({ input }) => {
      try {
        const { en, ar, ...categoryPayload } = input;

        const category = await prisma.category.create({
          data: categoryPayload,
        });
        if (!category) {
          throw new TRPCError({
            code: 'BAD_REQUEST',
            message: 'Category not created',
          });
        }
        const categoryDescPayload = [
          { ...en, category_id: category.id },
          { ...ar, category_id: category.id },
        ];

        const categoryDesc = await prisma.categoryDescription.createMany({
          data: categoryDescPayload,
        });

        if (!categoryDesc.count) {
          throw new TRPCError({
            code: 'BAD_REQUEST',
            message: 'Category Description not created',
          });
        }

        return { data: category, message: 'Category created' };
      } catch (error: any) {
        throw new TRPCError({
          code: 'INTERNAL_SERVER_ERROR',
          message: error?.message,
        });
      }
    }),
  delete: publicProcedure
    .input(deleteCategorySchema)
    .mutation(async ({ input }) => {
      try {
        const category = await prisma.category.update({
          where: { id: input.id },
          data: { is_deleted: true },
        });
        if (!category) {
          throw new TRPCError({
            code: 'BAD_REQUEST',
            message: 'Category not found',
          });
        }

        return { data: category, message: 'Category deleted' };
      } catch (error: any) {
        throw new TRPCError({
          code: 'INTERNAL_SERVER_ERROR',
          message: error?.message,
        });
      }
    }),
});
