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

      const chartData =
        await prisma.$queryRaw`SELECT DATE_FORMAT( \`order\`.created_at,'%Y-%m-%d') as name, CAST(COUNT(*) AS CHAR) as count,SUM(\`order\`.total_amount) as total,CAST(SUM(\`order\`.sub_total_amount) AS DECIMAL(10, 2)) as sub_total_amount,CAST(SUM(\`order\`.discount_amount) AS DECIMAL(10, 2)) as discount_amount
    FROM \`order\`
    GROUP BY name ORDER BY name`;

      return { message: 'Chart Data', data: chartData };
    } catch (error: any) {
      throw new TRPCError({
        code: 'INTERNAL_SERVER_ERROR',
        message: error?.message,
      });
    }
  }),
  recent: publicProcedure.query(async ({ ctx }) => {
    try {
      const token = ctx?.req?.cookies['winnar-admin-token'];

      let userData;
      if (token) {
        userData = await verifyJWT(token);
      } else {
        return { data: null };
      }
      const recent_orders = await prisma.order.findMany({
        take: 5,
        orderBy: {
          created_at: 'desc',
        },
        where: {
          is_deleted: false,
        },
        include: {
          Customer: {
            select: {
              first_name: true,
              last_name: true,
              email: true,
            },
          },
        },
      });

      return { message: 'Analytics Data', data: recent_orders };
    } catch (error: any) {
      throw new TRPCError({
        code: 'INTERNAL_SERVER_ERROR',
        message: error?.message,
      });
    }
  }),
});
