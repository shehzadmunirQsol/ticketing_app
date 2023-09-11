import { router, publicProcedure } from '../trpc';
import { TRPCError } from '@trpc/server';
import {
  addToCartSchema,
  getCartSchema,
  getTicketPurchasedSchema,
  removeCartItemSchema,
} from '~/schema/cart';
import { prisma } from '~/server/prisma';
import { verifyJWT } from '~/utils/jwt';

export const dashboardRouter = router({
  analytics: publicProcedure.query(async ({ ctx }) => {
    try {
      const token = ctx?.req?.cookies['winnar-admin-token'];

      let userData;
      if (token) {
        userData = await verifyJWT(token);
      } else {
        return { data: null };
      }
      const totalCustomerPromise = await prisma.customer.count({
        where: {
          is_deleted: false,
          is_approved: true,
        },
      });
      const pendingCustomerPromise = await prisma.customer.count({
        where: {
          is_approved: false,
        },
      });
      const todayDate = new Date();
      const couponExpiryPromise = await prisma.coupon.count({
        where: {
          is_enabled: true,
          is_deleted: false,
          start_date: { lte: todayDate },
          end_date: { gte: todayDate },
        },
      });
      const eventsPromise = await prisma.event.count({
        where: {
          is_deleted: false,
        },
      });
      const ordersAmountPromise = await prisma.order.aggregate({
        _sum: {
          total_amount: true,
        },
      });
      const [
        totalCustomer,
        pendingCustomer,
        couponExpiry,
        events,
        ordersAmount,
      ] = await Promise.all([
        totalCustomerPromise,
        pendingCustomerPromise,
        couponExpiryPromise,
        eventsPromise,
        ordersAmountPromise,
      ]);
      const analyticsData: any = [
        {
          title: 'Active Customer',
          data: totalCustomer,
          symbol: '',
          icon: 'fas fa-compass',
          cols: true,
        },
        {
          title: 'Pending Customer',
          data: pendingCustomer,
          symbol: '',
          icon: 'fas fa-compass',
          cols: true,
        },
        {
          title: 'Active Coupon',
          data: couponExpiry,
          symbol: '',
          icon: 'fas fa-compass',
          cols: true,
        },
        {
          title: 'Active Events',
          data: events,
          symbol: '',
          icon: 'fas fa-compass',
          cols: true,
        },
        {
          title: 'Order Amount',
          data: ordersAmount._sum.total_amount,
          symbol: 'AED',
          icon: 'fas fa-compass',
          cols: true,
        },
      ];

      return { message: 'Analytics Data', data: analyticsData };
    } catch (error: any) {
      throw new TRPCError({
        code: 'INTERNAL_SERVER_ERROR',
        message: error?.message,
      });
    }
  }),
  chart: publicProcedure.query(async ({ ctx }) => {
    try {
      const token = ctx?.req?.cookies['winnar-admin-token'];

      let userData;
      if (token) {
        userData = await verifyJWT(token);
      } else {
        return { data: null };
      }
      // const totalCustomerPromise = await prisma.order.groupBy({
      //   by: {
      //     month: {
      //       // Use the `extract` function to extract the month from the date column
      //       _in: prisma.$queryRaw`${prisma.order.created_at}.MONTH`,
      //     },
      //   },
      //   where: {
      //     is_deleted: false,
      //     is_approved: true,
      //   },
      // });
      const pendingCustomerPromise = await prisma.customer.count({
        where: {
          is_approved: false,
        },
      });

      return { message: 'Chart Data', data: pendingCustomerPromise };
    } catch (error: any) {
      throw new TRPCError({
        code: 'INTERNAL_SERVER_ERROR',
        message: error?.message,
      });
    }
  }),
});
