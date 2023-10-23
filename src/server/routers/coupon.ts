import { router, publicProcedure } from '../trpc';
import { TRPCError } from '@trpc/server';
import { z } from 'zod';
import {
  applyCouponSchema,
  createCouponSchema,
  getCouponSchema,
  updateCouponSchema,
  updateSchema,
  deleteCouponSchema,
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
        const todayDate = new Date()?.toISOString()?.split('T')[0] ?? '';
        const startDate =
          coupon?.start_date?.toISOString()?.split('T')[0] ?? '';
        const endDate = coupon?.end_date?.toISOString()?.split('T')[0] ?? '';
        const isExpired = todayDate < startDate || todayDate > endDate;

        if (isExpired) {
          throw new TRPCError({
            code: 'NOT_FOUND',
            message: 'Coupon Expired',
          });
        }

        const couponApply = await prisma.couponApply.findFirst({
          where: {
            customer_id: input?.customer_id,
            coupon_id: coupon?.id,
            is_deleted: false,
            is_used: false,
          },
        });
        if (couponApply) {
          throw new TRPCError({
            code: 'FORBIDDEN',
            message: 'Coupon Already Applied',
          });
        }

        if (coupon.is_limited) {
          const couponUsed = await prisma.couponApply.count({
            where: {
              customer_id: input?.customer_id,
              coupon_id: coupon?.id,
              is_deleted: false,
              is_used: true,
            },
          });

          if (coupon?.coupon_limit && couponUsed >= coupon?.coupon_limit) {
            throw new TRPCError({
              code: 'NOT_FOUND',
              message: 'Coupon Already Used',
            });
          }
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
      const { filters, ...payload } = input;
      const filterPayload: any = { ...filters };

      if (filterPayload?.searchQuery) delete filterPayload.searchQuery;
      if (filterPayload?.endDate) delete filterPayload.endDate;
      if (filterPayload?.startDate) delete filterPayload.startDate;
      const where: any = { is_deleted: false, ...filterPayload };

      if (input?.filters?.searchQuery) {
        where.OR = [];
        where.OR.push({
          name: {
            contains: input?.filters?.searchQuery,
            mode: 'insensitive',
          },
        });
        where.OR.push({
          coupon_code: {
            contains: input?.filters?.searchQuery,
            mode: 'insensitive',
          },
        });
      }

      if (input?.filters?.startDate && !input?.filters?.endDate) {
        const startDate = new Date(input?.filters?.startDate)
          ?.toISOString()
          .split('T')[0] as string;
        where.created_at = { gte: new Date(startDate) };
      }
      if (input?.filters?.endDate && !input?.filters?.startDate) {
        const endDate = new Date(input?.filters?.endDate)
          ?.toISOString()
          .split('T')[0] as string;
        where.created_at = { lte: new Date(endDate) };
      }
      if (input?.filters?.endDate && input?.filters?.startDate) {
        const startDate = new Date(input?.filters?.startDate)
          ?.toISOString()
          .split('T')[0] as string;
        const endDate = new Date(input?.filters?.endDate)
          ?.toISOString()
          .split('T')[0] as string;
        where.created_at = { gte: new Date(startDate), lte: new Date(endDate) };
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
        const isExist = await prisma.coupon.findFirst({
          where: {
            coupon_code: input?.coupon_code?.toUpperCase(),
            is_deleted: false,
          },
        });
        if (isExist) {
          throw new TRPCError({
            code: 'FORBIDDEN',
            message: 'Coupon code must be unique!',
          });
        }

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
        if (input?.coupon_limit) payload.coupon_limit = +input?.coupon_limit;
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
  updateCoupon: publicProcedure
    .input(updateSchema)
    .mutation(async ({ input }) => {
      try {
        const payload: any = {
          user_id: input?.user_id,
          name: input?.name,
          coupon_code: input?.coupon_code?.toUpperCase(),
          is_percentage: input?.is_percentage == '1' ? true : false,
          is_limited: input?.is_limited == '1' ? true : false,

          discount: input?.discount,
        };
        if (input?.coupon_limit) payload.coupon_limit = +input?.coupon_limit;
        const coupon = await prisma.coupon.update({
          where: { id: input?.coupon_id },
          data: { ...payload },
        });
        if (!coupon) {
          throw new TRPCError({
            code: 'NOT_FOUND',
            message: 'Invalid Coupon',
          });
        }

        return { message: 'Coupon Updated', data: coupon };
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

  delete: publicProcedure
    .input(deleteCouponSchema)
    .mutation(async ({ input }) => {
      try {
        console.log(input, 'INPUT::');
        const coupon = await prisma.coupon.update({
          where: { id: input.id },
          data: { is_deleted: true },
        });
        if (!coupon) {
          throw new TRPCError({
            code: 'BAD_REQUEST',
            message: 'Coupon not found',
          });
        }

        return { data: coupon, message: 'Coupon deleted' };
      } catch (error: any) {
        throw new TRPCError({
          code: 'INTERNAL_SERVER_ERROR',
          message: error?.message,
        });
      }
    }),
});
