import { z } from 'zod';

export const createBannerSchema = z.array(z.any());
export const getBannerSchema = z.object({
  startDate: z.date().nullable().optional(),
  endDate: z.date().nullable().optional(),
  lang_id: z.number().optional(),
  banner_id: z.number().optional(),
  searchQuery: z.string().optional(),
  page: z.number(),
  first: z.number(),
  rows: z.number(),
});
