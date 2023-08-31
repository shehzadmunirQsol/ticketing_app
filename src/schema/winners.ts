import { z } from 'zod';

export const getWinnersByIdSchema = z.object({
  first: z.number(),
  rows: z.number(),
  lang_id: z.number(),
});
