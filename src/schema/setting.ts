import { z } from 'zod';

export const createBannerSchema = z.array(z.any());
export const updateBannerSchema = z.any();
export const deleteBannerSchema = z.object({
  id: z.number(),
  is_deleted: z.boolean(),
});
export const getBannerSchema = z.object({
  startDate: z.date().nullable().optional(),
  endDate: z.date().nullable().optional(),
  group: z.string().optional(),
  lang_id: z.number().optional(),
  is_enabled: z.boolean().optional(),
  banner_id: z.number().optional(),
  searchQuery: z.string().optional(),
  first: z.number(),
  rows: z.number(),
  filters: z.any().optional(),
});
