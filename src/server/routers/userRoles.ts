import { router, publicProcedure } from '../trpc';
import { TRPCError } from '@trpc/server';
import { z } from 'zod';
import {
  addPermisionSchema,
  addResourcesSchema,
  getRolesPermisionSchema,
  getRolesSchema,
} from '~/schema/roles';
import { prisma } from '~/server/prisma';

export const rolesRouter = router({
  get: publicProcedure.input(getRolesSchema).query(async ({ input }) => {
    try {
      const { filters, ...payload } = input;
      const filterPayload: any = { ...filters };

      if (filterPayload?.searchQuery) delete filterPayload.searchQuery;
      if (filterPayload?.endDate) delete filterPayload.endDate;
      if (filterPayload?.startDate) delete filterPayload.startDate;
      const where: any = {
        is_deleted: false,
        ...filterPayload,
        name: {
          not: 'admin',
        },
      };

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

      const totalCategoryPromise = prisma.role.count({
        where: where,
      });

      const categoryPromise = prisma.role.findMany({
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
  getResources: publicProcedure
    .input(getRolesSchema)
    .query(async ({ input }) => {
      try {
        const { filters, ...payload } = input;
        const filterPayload: any = { ...filters };

        if (filterPayload?.searchQuery) delete filterPayload.searchQuery;
        if (filterPayload?.endDate) delete filterPayload.endDate;
        if (filterPayload?.startDate) delete filterPayload.startDate;
        const where: any = {
          is_deleted: false,
          ...filterPayload,
          name: {
            not: 'admin',
          },
        };

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
          where.created_at = {
            gte: new Date(startDate),
            lte: new Date(endDate),
          };
        }

        const totalCategoryPromise = prisma.resources.count({
          where: where,
        });

        const categoryPromise = prisma.resources.findMany({
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
  uploadResources: publicProcedure
    .input(addResourcesSchema)
    .mutation(async ({ input }) => {
      try {
        const { id, ...payload } = input;

        const resources = await prisma.resources.upsert({
          where: { id: id ?? 0 },
          create: {
            ...payload,
          },
          update: {
            ...payload,
          },
        });

        return {
          message: 'resource add successfully',
          data: resources,
        };
      } catch (error: any) {
        throw new TRPCError({
          code: 'INTERNAL_SERVER_ERROR',
          message: error?.message,
        });
      }
    }),
  getResourcesPermisions: publicProcedure
    .input(getRolesPermisionSchema)
    .query(async ({ input }) => {
      try {
        const { filters, ...payload } = input;
        const filterPayload: any = { ...filters };

        if (filterPayload?.searchQuery) delete filterPayload.searchQuery;
        if (filterPayload?.endDate) delete filterPayload.endDate;
        if (filterPayload?.startDate) delete filterPayload.startDate;
        const where: any = {
          is_deleted: false,
          ...filterPayload,
          name: {
            not: 'admin',
          },
        };

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
          where.created_at = {
            gte: new Date(startDate),
            lte: new Date(endDate),
          };
        }

        const totalCategoryPromise = prisma.resources.count({
          where: where,
        });

        const resourcePromise = prisma.resources.findMany({
          orderBy: { created_at: 'asc' },

          where: where,
        });

        const permisionPromise = prisma.rolePermission.findMany({
          orderBy: { created_at: 'asc' },

          where: {
            role_id: input?.id ?? 0,
          },
          include: {
            Resources: true,
          },
        });
        const [totalCategory, resources, access] = await Promise.all([
          totalCategoryPromise,
          resourcePromise,
          permisionPromise,
        ]);

        if (!resources?.length) {
          throw new TRPCError({
            code: 'NOT_FOUND',
            message: 'Categories not found',
          });
        }
        const frequencyCounter = resources.reduce((accu: any, curr: any) => {
          const find_access = access?.find(
            (item) => item?.resource_id === curr?.id,
          );
          console.log({ find_access });
          accu[curr.id] = find_access ? find_access?.access : 'N';
          return accu;
        }, {});

        return {
          message: 'categories found',
          count: totalCategory,
          data: resources,
          switch: frequencyCounter,
          access,
        };
      } catch (error: any) {
        throw new TRPCError({
          code: 'INTERNAL_SERVER_ERROR',
          message: error?.message,
        });
      }
    }),
  uploadPermisions: publicProcedure
    .input(addPermisionSchema)
    .mutation(async ({ input }) => {
      try {
        const { ...payload } = input;
        if (input) {
          await input?.map(async (item, index) => {
            console.log(item, index);
            if (item?.resource_id === 0 || item?.role_id === 0) {
              throw new TRPCError({
                code: 'NOT_FOUND',
                message: 'Something Went Wrong!',
              });
            }
            const findData = await prisma.rolePermission.findFirst({
              where: {
                resource_id: item?.resource_id ? item?.resource_id : 0,
                role_id: item?.role_id ? item?.role_id : 0,
              },
            });
            if (findData) {
              await prisma.rolePermission.update({
                where: {
                  id: findData?.id,
                },
                data: {
                  access: item?.access,
                },
              });
            } else {
              await prisma.rolePermission.create({
                data: {
                  ...item,
                },
              });
            }
          });
        }

        return {
          message: 'resource add successfully',
          // data: resources,
        };
      } catch (error: any) {
        throw new TRPCError({
          code: 'INTERNAL_SERVER_ERROR',
          message: error?.message,
        });
      }
    }),
});
