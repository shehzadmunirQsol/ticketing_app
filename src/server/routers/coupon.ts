import { router, publicProcedure } from '../trpc';
import { TRPCError } from '@trpc/server';
import { z } from 'zod';
import {
  applyCouponSchema,
  createCouponSchema,
  getCouponSchema,
  updateCouponSchema,
} from '~/schema/coupon';
import { prisma } from '~/server/prisma';

export const couponRouter = router({
  applyCoupon: publicProcedure
    .input(applyCouponSchema)
    .mutation(async ({ input }) => {
      try {
        const coupon = await prisma.coupon.findFirst({
          where: {
            coupon_code: { contains: input?.coupon_code },
            is_enabled: true,
            is_deleted: false,
          },
        });
        if (!coupon) {
          throw new TRPCError({
            code: 'NOT_FOUND',
            message: 'Invalid Coupon',
          });
        }
        const todayDate = new Date();
        const couponExpiry = await prisma.coupon.findFirst({
          where: {
            coupon_code: input?.coupon_code,
            is_enabled: true,
            is_deleted: false,
            start_date: { lte: todayDate },
            end_date: { gte: todayDate },
          },
        });
        console.log({ couponExpiry });
        if (!couponExpiry) {
          throw new TRPCError({
            code: 'NOT_FOUND',
            message: 'Coupon Expired',
          });
        }
        if (couponExpiry && coupon) {
          const check = await prisma.couponApply.findFirst({
            where: {
              customer_id: input?.customer_id,
              coupon_id: coupon?.id,
            },
          });
          if (check) {
            throw new TRPCError({
              code: 'NOT_FOUND',
              message: 'Coupon Already Used',
            });
          }
          const createCouponApplied = await prisma.couponApply.create({
            data: {
              coupon_id: coupon?.id,
              customer_id: input?.customer_id,
              cart_id: input?.cart_id,
              discount: coupon?.discount,
              is_percentage: coupon?.is_percentage,
            },
          });

          return { message: 'Coupon Applied', data: createCouponApplied };
        }
      } catch (error: any) {
        throw new TRPCError({
          code: 'INTERNAL_SERVER_ERROR',
          message: error?.message,
        });
      }
    }),
  getCoupon: publicProcedure
    .input(applyCouponSchema)
    .query(async ({ input }) => {
      try {
        const coupon = await prisma.couponApply.findFirst({
          where: {
            customer_id: input?.customer_id,
            cart_id: input?.cart_id,
            is_deleted: false,
          },
          include: {
            Coupon: true,
          },
        });
        if (!coupon) {
          throw new TRPCError({
            code: 'NOT_FOUND',
            message: 'Invalid Coupon',
          });
        }

        return { message: 'Coupon Applied', data: coupon };
      } catch (error: any) {
        throw new TRPCError({
          code: 'INTERNAL_SERVER_ERROR',
          message: error?.message,
        });
      }
    }),
  getById: publicProcedure
    .input(updateCouponSchema)
    .query(async ({ input }) => {
      try {
        const coupon = await prisma.coupon.findFirst({
          where: {
            id: input?.coupon_id,
            is_deleted: false,
          },
        });
        if (!coupon) {
          throw new TRPCError({
            code: 'NOT_FOUND',
            message: 'Invalid Coupon',
          });
        }

        return { message: 'Coupon Applied', data: coupon };
      } catch (error: any) {
        throw new TRPCError({
          code: 'INTERNAL_SERVER_ERROR',
          message: error?.message,
        });
      }
    }),
  get: publicProcedure.input(getCouponSchema).query(async ({ input }) => {
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

      const totalCategoryPromise = prisma.coupon.count({
        where: where,
      });

      const categoryPromise = prisma.coupon.findMany({
        orderBy: { created_at: 'asc' },
        skip: input.first * input.rows,
        take: input.rows,
        where: where,
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
    .input(createCouponSchema)
    .mutation(async ({ input }) => {
      try {
        const payload: any = {
          user_id: input?.user_id,
          name: input?.name,
          coupon_code: input?.coupon_code?.toUpperCase(),
          is_percentage: input?.is_percentage == '1' ? true : false,
          is_limited: input?.is_limited == '1' ? true : false,

          discount: input?.discount,
          start_date: input?.start_date,
          end_date: input?.end_date,
        };
        if (input?.limit) payload.limit = +input?.limit;
        const coupon = await prisma.coupon.create({
          data: { ...payload },
        });
        if (!coupon) {
          throw new TRPCError({
            code: 'NOT_FOUND',
            message: 'Invalid Coupon',
          });
        }

        if (!coupon) {
          throw new TRPCError({
            code: 'NOT_FOUND',
            message: 'Coupon Expired',
          });
        }

        return { message: 'Coupon Created', data: coupon };
      } catch (error: any) {
        throw new TRPCError({
          code: 'INTERNAL_SERVER_ERROR',
          message: error?.message,
        });
      }
    }),
  update: publicProcedure
    .input(updateCouponSchema)
    .mutation(async ({ input }) => {
      const { coupon_id, ...payload } = input;
      const setting_banner = await prisma.coupon.update({
        where: { id: coupon_id },
        data: payload,
      });
      return setting_banner;
    }),
});
