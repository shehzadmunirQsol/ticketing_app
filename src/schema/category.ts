import { z } from 'zod';

export const getCategorySchema = z.object({
  startDate: z.date().nullable(),
  endDate: z.date().nullable(),
  category_id: z.string(),
  searchQuery: z.string(),
  page: z.number(),
  first: z.number(),
  rows: z.number(),
});

export const createCategorySchema = z.object({
  thumb: z.string(),
  creator_id: z.number(),

  en: z.object({
    name: z.string(),
    desc: z.string().optional(),
    lang_id: z.number(),
  }),
  ar: z.object({
    name: z.string(),
    desc: z.string().optional(),
    lang_id: z.number(),
  }),
});

export const deleteCategorySchema = z.object({
  id: z.number(),
});
