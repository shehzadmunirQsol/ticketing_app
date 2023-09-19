import { z } from 'zod';

export const getWinnersSchema = z.object({
  first: z.number(),
  rows: z.number(),
  lang_id: z.number().default(1),
  filters: z.any().optional(),
});

export const selectWinnerSchema = z.object({
  event_id: z.number(),
  customer_id: z.number(),
  customer_email: z.string().email(),
  customer_name: z.string(),
  event_name: z.string(),
});
