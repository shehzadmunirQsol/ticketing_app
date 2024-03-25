import { router, publicProcedure } from '../trpc';
import { TRPCError } from '@trpc/server';
import { projectGetAdminchema, projectGetDetailSchema, projectGetTicketDetailSchema } from '~/schema/project';

import { prisma } from '~/server/prisma';

export const projectRouter = router({
  getInvoiceTickets: publicProcedure
    .input(projectGetTicketDetailSchema)
    .query(async ({ input }) => {
      try {
        console.log("first")
        console.log(input?.id,"input?.id")
        const where: any = {
          is_deleted: false,
          is_invoiced:true,
        };

        const projectPromise = prisma.projects.findFirst({
          orderBy: { created_at: 'asc' },

          where: { ...where, id: input?.id },
          include: {
            Client:{
              select:{
                first_name:true,
                username:true,
                email:true,
                phone_number:true
              }
            },
            User:{
              select:{
                first_name:true,
                username:true,
                email:true,
                phone_number:true
              }
            },
            ProjectTickets:{
              select:{
                trucker_id:true,
                tx_hash:true,
                Trucker:true,
                status:true,
              }
            },
            ProjectAddress:true
          },
        });

        const [projectDetail] = await Promise.all([projectPromise]);

        return {
          message: 'Detail found',
          data: projectDetail,
        };
      } catch (error: any) {
        throw new TRPCError({
          code: 'INTERNAL_SERVER_ERROR',
          message: error?.message,
        });
      }
    }),
  get: publicProcedure.input(projectGetAdminchema).query(async ({ input }) => {
    try {
      const { filters, ...payload } = input;
      const filterPayload: any = { ...filters };

      if (filterPayload?.searchQuery) delete filterPayload.searchQuery;
      if (filterPayload?.endDate) delete filterPayload.endDate;
      if (filterPayload?.startDate) delete filterPayload.startDate;
      const where: any = {
        is_deleted: false,
        is_invoiced: input?.type === 'active' ? false : true,
        ...filterPayload,
      };

      if (input?.filters?.searchQuery) {
        where.OR = [];
        where.OR.push({
          first_name: {
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

      const totalProjectsPromise = prisma.projects.count({
        where: where,
      });

      const projectPromise = prisma.projects.findMany({
        orderBy: { created_at: 'asc' },
        skip: input.first * input.rows,
        take: input.rows,
        where: where,
        include: {
          User: {
            select: {
              username: true,
              email: true,
              profile_pic: true,
            },
          },
          Client: {
            select: {
              username: true,
              email: true,
              profile_pic: true,
            },
          },
        },
      });

      const [totalCustomers, customers] = await Promise.all([
        totalProjectsPromise,
        projectPromise,
      ]);

      if (!customers?.length) {
        throw new TRPCError({
          code: 'NOT_FOUND',
          message: 'Categories not found',
        });
      }

      return {
        message: 'categories found',
        count: totalCustomers,
        data: customers,
      };
    } catch (error: any) {
      throw new TRPCError({
        code: 'INTERNAL_SERVER_ERROR',
        message: error?.message,
      });
    }
  }),
  getDetail: publicProcedure
    .input(projectGetDetailSchema)
    .query(async ({ input }) => {
      try {
        const where: any = {
          is_deleted: false,
        };

        const projectPromise = prisma.projects.findFirst({
          orderBy: { created_at: 'asc' },

          where: { ...where, id: input?.id },
          include: {
            User: {
              select: {
                username: true,
                email: true,
                profile_pic: true,
              },
            },
            Client: {
              select: {
                username: true,
                email: true,
                profile_pic: true,
              },
            },
          },
        });

        const [projectDetail] = await Promise.all([projectPromise]);

        return {
          message: 'Detail found',
          data: projectDetail,
        };
      } catch (error: any) {
        throw new TRPCError({
          code: 'INTERNAL_SERVER_ERROR',
          message: error?.message,
        });
      }
    }),
  getProjectTruckers: publicProcedure
    .input(projectGetAdminchema)
    .query(async ({ input }) => {
      try {
        const { filters, ...payload } = input;
        const filterPayload: any = { ...filters };

        if (filterPayload?.searchQuery) delete filterPayload.searchQuery;
        if (filterPayload?.endDate) delete filterPayload.endDate;
        if (filterPayload?.startDate) delete filterPayload.startDate;

        if (!input?.id) {
          throw new TRPCError({
            code: 'NOT_FOUND',
            message: 'Id was not found in payload',
          });
        }
        const where: any = {
          is_deleted: false,
          project_id: input?.id,

          ...filterPayload,
        };

        if (input?.filters?.searchQuery) {
          where.OR = [];
          where.OR.push({
            first_name: {
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
          where.created_at = {
            gte: new Date(startDate),
            lte: new Date(endDate),
          };
        }

        const totalProjectsPromise = prisma.projectTruckers.count({
          where: where,
        });

        const projectPromise = prisma.projectTruckers.findMany({
          orderBy: { created_at: 'asc' },
          skip: input.first * input.rows,
          take: input.rows,
          where: where,
          include: {
            Projects: true,

            Trucker: {
              include: {
                _count: {
                  select: {
                    ProjectTickets: {
                      where: {
                        project_id: input?.id,
                      },
                    },
                  },
                },
              },
            },
          },
        });

        const [totalCustomers, customers] = await Promise.all([
          totalProjectsPromise,
          projectPromise,
        ]);

        if (!customers?.length) {
          throw new TRPCError({
            code: 'NOT_FOUND',
            message: 'Categories not found',
          });
        }

        return {
          message: 'categories found',
          count: totalCustomers,
          data: customers,
        };
      } catch (error: any) {
        throw new TRPCError({
          code: 'INTERNAL_SERVER_ERROR',
          message: error?.message,
        });
      }
    }),
  getProjectTickets: publicProcedure
    .input(projectGetAdminchema)
    .query(async ({ input }) => {
      try {
        const { filters, id, ...payload } = input;
        const filterPayload: any = { ...filters };

        if (filterPayload?.searchQuery) delete filterPayload.searchQuery;
        if (filterPayload?.endDate) delete filterPayload.endDate;
        if (filterPayload?.startDate) delete filterPayload.startDate;

        const where: any = {
          is_deleted: false,

          ...filterPayload,
        };
        if (!id) {
          where.project_id = id;
        }
        if (input?.filters?.searchQuery) {
          where.OR = [];
          where.OR.push({
            first_name: {
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
          where.created_at = {
            gte: new Date(startDate),
            lte: new Date(endDate),
          };
        }

        const totalProjectsPromise = prisma.projectTickets.count({
          where: where,
        });

        const projectPromise = prisma.projectTickets.findMany({
          orderBy: { created_at: 'asc' },
          skip: input.first * input.rows,
          take: input.rows,
          where: where,
          include: {
            Projects: true,

            Trucker: {
              include: {
                _count: {
                  select: {
                    ProjectTickets: {
                      where: {
                        project_id: id as number,
                      },
                    },
                  },
                },
              },
            },
          },
        });

        const [totalCustomers, customers] = await Promise.all([
          totalProjectsPromise,
          projectPromise,
        ]);

        if (!customers?.length) {
          throw new TRPCError({
            code: 'NOT_FOUND',
            message: 'Categories not found',
          });
        }

        return {
          message: 'categories found',
          count: totalCustomers,
          data: customers,
        };
      } catch (error: any) {
        throw new TRPCError({
          code: 'INTERNAL_SERVER_ERROR',
          message: error?.message,
        });
      }
    }),
});
