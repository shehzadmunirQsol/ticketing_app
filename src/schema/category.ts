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
    name: z
      .string()
      .min(2, {
        message: 'Name must be at least 2 characters',
      })
      .max(24, {
        message: 'Name must not exceed 24 characters',
      }),
    desc: z
      .string()
      .min(6, {
        message: 'Description must be at least 2 characters',
      })
      .max(5000, {
        message: 'Description must not exceed 5000 characters',
      })
      .optional(),
    lang_id: z.number(),
  }),
  ar: z.object({
    name: z
      .string()
      .min(2, {
        message: 'Name must be at least 2 characters',
      })
      .max(24, {
        message: 'Name must not exceed 24 characters',
      }),
    desc: z
      .string()
      .min(6, {
        message: 'Description must be at least 2 characters',
      })
      .max(5000, {
        message: 'Description must not exceed 5000 characters',
      }),
    lang_id: z.number(),
  }),
});

export const deleteCategorySchema = z.object({
  id: z.number(),
});

export type CreateCategorySchema = z.infer<typeof createCategorySchema>;