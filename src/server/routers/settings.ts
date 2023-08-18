import {
  createBannerSchema,
  getBannerSchema,
  updateBannerSchema,
} from '~/schema/setting';
import { router, publicProcedure } from '../trpc';
import { TRPCError } from '@trpc/server';
import { loginSchema, otpSchema } from '~/schema/user';
import { prisma } from '~/server/prisma';
import { generateOTP } from '~/utils/helper';

export const settingRouter = router({
  banner_create: publicProcedure
    .input(createBannerSchema)
    .mutation(async ({ input, ctx }) => {
      // const payload = [...input];
      console.log(input, 'inputinputinputinput');
      const setting_banner = await prisma.setting.createMany({
        data: input,
        skipDuplicates: true,
      });
      return setting_banner;
    }),
  get_banner: publicProcedure
    .input(getBannerSchema)
    .query(async ({ input }) => {
      try {
        const options: any = {
          orderBy: { created_at: 'desc' },
          skip: input.first,
          take: input.rows,
          where: {
            group: 'BANNER',
            is_deleted: false,
          },
        };
        if (input?.lang_id) {
          options.where = {
            lang_id: input?.lang_id,
            group: 'BANNER',
            is_deleted: false,
          };
        }
        if (input?.banner_id) {
          options.where = {
            id: input?.banner_id,
            group: 'BANNER',
            is_deleted: false,
          };
        }
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
        const setting_banner = await prisma.setting.findMany({
          ...options,
        });
        console.log({ options });
        return setting_banner;
      } catch (error: any) {
        throw new TRPCError({
          code: 'INTERNAL_SERVER_ERROR',
          message: error.message,
        });
      }
    }),

  banner_update: publicProcedure
    .input(updateBannerSchema)
    .mutation(async ({ input, ctx }) => {
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
});
