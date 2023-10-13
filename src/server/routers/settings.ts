import {
  createBannerSchema,
  deleteBannerSchema,
  getBannerSchema,
  updateBannerSchema,
} from '~/schema/setting';
import { router, publicProcedure } from '../trpc';
import { TRPCError } from '@trpc/server';
import { prisma } from '~/server/prisma';

export const settingRouter = router({
  banner_create: publicProcedure
    .input(createBannerSchema)
    .mutation(async ({ input }) => {
      const {
        en,
        ar,

        ...settingPayload
      }: any = input;
      const payload = [{ ...en }, { ...ar }];
      console.log(input, 'inputinputinputinput');
      const setting_banner = await prisma.setting.create({
        data: {
          ...settingPayload,
          SettingDescription: { createMany: { data: payload } },
        },
      });
      return setting_banner;
    }),
  banner_update: publicProcedure
    .input(updateBannerSchema)
    .mutation(async ({ input }) => {
      // const payload = [...input];
      const {
        en,
        ar,

        id = 0,
        ...eventPayload
      } = input;
      console.log(input, 'inputinputinputinput');
      const setting_banner = await prisma.setting.update({
        where: {
          id: input?.id,
        },
        data: { ...eventPayload },
      });
      const eventEnPromise = await prisma.settingDescription.updateMany({
        where: { setting_id: input?.id, lang_id: 1 },
        data: en,
      });

      const eventArPromise = await prisma.settingDescription.updateMany({
        where: { setting_id: input?.id, lang_id: 2 },
        data: ar,
      });
      return setting_banner;
    }),
  banner_delete: publicProcedure
    .input(deleteBannerSchema)
    .mutation(async ({ input }) => {
      // const payload = [...input];
      const payload: any = { ...input };
      if (input?.id) delete payload?.id;
      console.log(input, 'inputinputinputinput');
      const setting_banner = await prisma.setting.update({
        where: {
          id: input?.id,
        },
        data: { ...payload },
      });
      return setting_banner;
    }),
  get_banner: publicProcedure
    .input(getBannerSchema)
    .query(async ({ input }) => {
      try {
        const { filters, ...inputData } = input;
        const filterPayload: any = { ...filters };

        if (filterPayload?.searchQuery) delete filterPayload.searchQuery;
        if (filterPayload?.endDate) delete filterPayload.endDate;
        if (filterPayload?.startDate) delete filterPayload.startDate;
        const options: any = {
          orderBy: { created_at: 'desc' },
          skip: input.first * input.rows,
          take: input.rows,
          where: {
            group: input?.group,
            is_deleted: false,
            ...filterPayload,
          },
        };
        const select: any = {
          select: {
            id: true,
            title: true,
            link: true,
            thumb: true,
            model: true,
            value: true,
            price: true,
            lang_id: true,
            description: true,
            is_enabled: true,
            is_deleted: true,
            date: true,
            created_at: true,
          },
        };

        if (input?.group == 'WONDER') {
          select.select = {
            id: true,
            lang_id: true,
            value: true,
            name: true,
            link: true,
            description: true,
            is_enabled: true,
            is_deleted: true,
            thumb: true,
            created_at: true,
          };
        }

        // Filters implementations
        if (input?.filters?.searchQuery) {
          options.where.OR = [];
          if (input?.group == 'WONDER') {
            options.where.OR.push({
              name: {
                contains: input?.filters?.searchQuery,
                mode: 'insensitive',
              },
            });
            options.where.OR.push({
              description: {
                contains: input?.filters?.searchQuery,
                mode: 'insensitive',
              },
            });
          } else {
            options.where.OR.push({
              model: {
                contains: input?.filters?.searchQuery,
                mode: 'insensitive',
              },
            });
            options.where.OR.push({
              title: {
                contains: input?.filters?.searchQuery,
                mode: 'insensitive',
              },
            });
            options.where.OR.push({
              description: {
                contains: input?.filters?.searchQuery,
                mode: 'insensitive',
              },
            });
          }
        }

        if (input?.filters?.startDate && !input?.filters?.endDate) {
          const startDate = new Date(input?.filters?.startDate)
            ?.toISOString()
            .split('T')[0] as string;
            options.where.created_at = { gte: new Date(startDate) };
        }
        if (input?.filters?.endDate && !input?.filters?.startDate) {
          const inputEndDate = new Date(input?.filters?.endDate);
          const endDate = new Date(inputEndDate.setHours(23, 59));
          options.where.created_at = { lte: endDate };
        }
        if (input?.filters?.endDate && input?.filters?.startDate) {
          const startDate = new Date(input?.filters?.startDate)
            ?.toISOString()
            .split('T')[0] as string;
          const inputEndDate = new Date(input?.filters?.endDate);
          const endDate = new Date(inputEndDate.setHours(23, 59));
          options.where.created_at = { gte: new Date(startDate), lte: endDate };
        }



        if (input?.lang_id) options.where.lang_id = input?.lang_id;

        if (input?.is_enabled) options.where.is_enabled = true;

        if (input?.banner_id) options.where.id = input?.banner_id;

        const totalBannerPromise = prisma.bannerView1.count({
          where: options?.where,
        });
        const bannerPromise = prisma.bannerView1.findMany({
          ...options,
          ...select,
        });
        const [totalBanner, banner] = await Promise.all([
          totalBannerPromise,
          bannerPromise,
        ]);
        if (!banner?.length) {
          throw new TRPCError({
            code: 'NOT_FOUND',
            message: 'banner not found',
          });
        } 

        return {
          message: 'banner found',
          count: totalBanner,
          data: banner,
        };
      } catch (error: any) {
        console.log({ error });
        throw new TRPCError({
          code: 'INTERNAL_SERVER_ERROR',
          message: error.message,
        });
      }
    }),
});
