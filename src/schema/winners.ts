import { z } from 'zod';

export const getWinnersSchema = z.object({
  first: z.number(),
  rows: z.number(),
  lang_id: z.number().default(1),
});

export const selectWinnerSchema = z.object({
  event_id: z.number(),
  customer_id: z.number(),
});
