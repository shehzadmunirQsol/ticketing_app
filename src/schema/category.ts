import { z } from 'zod';

export const createCategorySchema = z.object({
  thumb: z.string(),
  creator_id: z.number(),
  lang_id: z.number(),
  name: z.string(),
  desc: z.string().optional(),
});
