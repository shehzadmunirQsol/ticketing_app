import { z } from 'zod';

export const createBannerSchema = z.array(z.any());
export const getBannerSchema = z.object({
  startDate: z.date().nullable(),
  endDate: z.date().nullable(),
  lang_id:z.number(),
  searchQuery: z.string(),
  page: z.number(),
  first: z.number(),
  rows: z.number(),
});
