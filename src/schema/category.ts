import { z } from 'zod';

export const getCategorySchema = z.object({
  startDate: z.date().optional(),
  endDate: z.date().optional(),
  searchQuery: z.string().optional(),
  category_id: z.number().optional(),
  first: z.number(),
  rows: z.number(),
  lang_id: z.number(),
});
export const getCategoryEventSchema = z.object({
  category_id: z.number().optional(),

  lang_id: z.number(),
});
export const createCategorySchema = z.object({
  thumb: z.string(),
  user_id: z.number(),

  en: z.object({
    name: z
      .string()
      .min(2, {
        message: 'Name must be at least 2 characters',
      })
      .max(24, {
        message: 'Name must not exceed 24 characters',
      }).trim(),
    desc: z
      .string()
      .min(6, {
        message: 'Description must be at least 2 characters',
      })
      .max(5000, {
        message: 'Description must not exceed 5000 characters',
      }).trim()
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
      }).trim(),
    desc: z
      .string()
      .min(6, {
        message: 'Description must be at least 2 characters',
      })
      .max(5000, {
        message: 'Description must not exceed 5000 characters',
      }).trim(),
    lang_id: z.number(),
  }),
});
export const updateCategorySchema = z.object({
  category_id: z.number(),
  thumb: z.string(),
  user_id: z.number(),

  en: z.object({
    name: z
      .string()
      .min(2, {
        message: 'Name must be at least 2 characters',
      })
      .max(24, {
        message: 'Name must not exceed 24 characters',
      }).trim(),
    desc: z
      .string()
      .min(6, {
        message: 'Description must be at least 2 characters',
      })
      .max(5000, {
        message: 'Description must not exceed 5000 characters',
      }).trim()
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
      }).trim(),
    desc: z
      .string()
      .min(1, {
        message: 'Please add a description',
      })
      .max(500, {
        message: 'Description is too big',
      }).trim(),
    lang_id: z.number(),
  }),
});

export const categoryIdSchema = z.object({
  category_id: z.number(),
});

export type GetCategorySchema = z.infer<typeof getCategorySchema>;
export type CreateCategorySchema = z.infer<typeof createCategorySchema>;
