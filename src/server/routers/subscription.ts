import { router, publicProcedure } from '../trpc';
import { TRPCError } from '@trpc/server';
import { getOrderSchema } from '~/schema/order';

import { prisma } from '~/server/prisma';

export const subscriptionRouter = router({
  get: publicProcedure.input(getOrderSchema).query(async ({ input }) => {
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

      const totalSubscriptionPromise = prisma.orderSubscription.count({
        where: where,
      });

      const subscriptionPromise = prisma.orderSubscription.findMany({
        orderBy: { created_at: 'desc' },
        skip: input.first * input.rows,
        take: input.rows,
        where: where,
        include: {
          Customer: {
            select: {
              email: true,
              first_name: true,
              last_name: true,
            },
          },
          Event: {
            include: {
              EventDescription: true,
            },
          },
        },
      });

      const [totalSubs, subscription] = await Promise.all([
        totalSubscriptionPromise,
        subscriptionPromise,
      ]);

      if (!subscription?.length) {
        throw new TRPCError({
          code: 'NOT_FOUND',
          message: 'Events not found',
        });
      }

      return {
        message: 'events found',
        count: totalSubs,
        data: subscription,
      };
    } catch (error: any) {
      throw new TRPCError({
        code: 'INTERNAL_SERVER_ERROR',
        message: error?.message,
      });
    }
  }),
});
