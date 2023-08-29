import { router, publicProcedure } from '../trpc';
import { TRPCError } from '@trpc/server';
import { addToCartSchema, getCartSchema } from '~/schema/cart';
import { applyCouponSchema } from '~/schema/coupon';
import { prisma } from '~/server/prisma';

export const couponRouter = router({
  applyCoupon: publicProcedure
    .input(applyCouponSchema)
    .query(async ({ input }) => {
      try {
        const coupon = await prisma.coupon.findFirst({
          where: {
            coupon_code: input?.coupon_code,
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
            start_date: { gte: todayDate },
            end_date: { lte: todayDate },
          },
        });
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
});
