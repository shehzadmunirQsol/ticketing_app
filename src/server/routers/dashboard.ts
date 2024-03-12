import { router, publicProcedure } from '../trpc';
import { TRPCError } from '@trpc/server';

import { prisma } from '~/server/prisma';
import { verifyJWT } from '~/utils/jwt';

export const dashboardRouter = router({
  analytics: publicProcedure.query(async ({ ctx }) => {
    try {
      const token = ctx?.req?.cookies['ticketing-admin-token'];

      let userData;
      if (token) {
        userData = await verifyJWT(token);
      } else {
        return { data: null };
      }
      const totalSellerPromise = await prisma.user.count({
        where: {
          is_deleted: false,
          Role: {
            name: {
              in: 'seller',
            },
          },
        },
      });
      const totalTruckerPromise = await prisma.user.count({
        where: {
          is_deleted: false,
          Role: {
            name: {
              in: 'trucker',
            },
          },
        },
      });
      const totalClientPromise = await prisma.user.count({
        where: {
          is_deleted: false,
          Role: {
            name: {
              in: 'client',
            },
          },
        },
      });

      const [totalSeller, totalTrucker, totalClient] = await Promise.all([
        totalSellerPromise,
        totalTruckerPromise,
        totalClientPromise,
      ]);
      const date = new Date().toISOString().split('T')[0];
      const analyticsData: any = [
        {
          title: 'Active Sellers',
          data: totalSeller,
          symbol: '',
          icon: 'fas fa-users',
          cols: false,
          link: '/admin/customers?type=seller',
        },
        // {
        //   title: 'Pending Customers',
        //   data: pendingCustomer,
        //   symbol: '',
        //   icon: 'fa-solid fa-user-clock',
        //   cols: true,
        //   link: '/admin/customers?is_verified=false&is_disabled=false',
        // },
        {
          title: 'Active Truckers',
          data: totalTrucker,
          symbol: '',
          icon: 'fa-solid fa-truck-pickup',
          cols: false,
          link: '/admin/customers?type=trucker',
        },
        {
          title: 'Active Clients',
          data: totalClient,
          symbol: '',
          icon: 'fas fa-users',
          cols: false,
          link: `/admin/customers?type=client`,
        },
        {
          title: 'Active Projects',
          data: totalClient,
          symbol: '',
          icon: 'fa-solid fa-diagram-project',
          cols: false,
          link: `/admin/events?status=active`,
        },
        {
          title: 'Closed Projects',
          data: totalClient,
          symbol: '',
          icon: 'fa-solid fa-diagram-project',
          cols: false,
          link: `/admin/events?status=active`,
        },
        {
          title: 'Active Tickets',
          data: totalClient,
          symbol: '',
          icon: 'fa-solid fa-ticket',
          cols: false,
          link: `/admin/events?status=active`,
        },
        {
          title: 'Closed Tickets',
          data: totalClient,
          symbol: '',
          icon: 'fa-solid fa-ticket-simple',
          cols: false,
          link: `/admin/events?status=active`,
        },
        // {
        //   title: 'Sales Volume',
        //   data: ordersAmount._sum.total_amount,
        //   symbol: 'AED',
        //   icon: 'fa-solid fa-chart-line',
        //   cols: true,
        //   link: '/admin/orders',
        // },
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

      const chartData: any =
        await prisma.$queryRaw`SELECT to_char( o.created_at,'yyyy-mm-dd') as NAME,CAST(COUNT(*) AS CHAR) as count,SUM(o.total_amount) as total,CAST(SUM(o.sub_total_amount) AS DECIMAL(10, 2)) as sub_total_amount,CAST(SUM(o.discount_amount) AS DECIMAL(10, 2)) as discount_amount FROM PUBLIC."order" o GROUP BY  name order by NAME desc limit 7`;

      return {
        message: 'Chart Data',
        data: chartData.length ? chartData?.reverse() : [],
      };
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
      const recent_orders = await prisma.projects.findMany({
        take: 5,
        orderBy: {
          created_at: 'desc',
        },
        where: {
          is_deleted: false,
        },
        include: {
          User: {
            select: {
              first_name: true,
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
