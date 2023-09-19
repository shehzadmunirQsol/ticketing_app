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
      // const payload = [...input];
      console.log(input, 'inputinputinputinput');
      const setting_banner = await prisma.setting.createMany({
        data: input,
        skipDuplicates: true,
      });
      return setting_banner;
    }),
  banner_update: publicProcedure
    .input(updateBannerSchema)
    .mutation(async ({ input }) => {
      // const payload = [...input];
      const payload = { ...input };
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
        const options: any = {
          orderBy: { created_at: 'desc' },
          skip: input.first * input.rows,
          take: input.rows,
          where: {
            group: input?.group,
            is_deleted: false,
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
          };
        }
        if (input?.lang_id) options.where.lang_id = input?.lang_id;

        if (input?.is_enabled) options.where.is_enabled = true;

        if (input?.banner_id) options.where.id = input?.banner_id;

        if (input.startDate) {
          const startDate = new Date(input?.startDate);
          startDate.setDate(startDate.getDate());

          options.where.AND = [];
          options.where.AND.push({ created_at: { gte: startDate } });
        }
        if (input.endDate) {
          const endDate = new Date(input?.endDate);
          endDate.setDate(endDate.getDate() + 1);

          options.where.AND = options?.AND ?? [];
          options.where.AND.push({ created_at: { lte: endDate } });
        }
        const totalBannerPromise = prisma.bannerView.count({
          where: options?.where,
        });
        const bannerPromise = prisma.bannerView.findMany({
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
            message: 'Events not found',
          });
        }

        return {
          message: 'banner found',
          count: totalBanner,
          data: banner,
        };
      } catch (error: any) {
        throw new TRPCError({
          code: 'INTERNAL_SERVER_ERROR',
          message: error.message,
        });
      }
    }),
});
