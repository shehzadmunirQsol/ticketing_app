import { z } from 'zod';

export const getWinnersSchema = z.object({
  is_admin: z.boolean().default(false),
  first: z.number(),
  rows: z.number(),
  lang_id: z.number().default(1),
  filters: z.any().optional(),
});

export const selectWinnerSchema = z.object({
  event_id: z.number(),
  customer_id: z.number(),
  ticket_num: z.number(),
  customer_email: z.string().email(),
  customer_name: z.string(),
  event_name: z.string(),
  draw_date: z.date().optional(),
  is_winner_selected: z.boolean().optional(),
});

export const updateWinnerSchema = z.object({
  winner_id: z.number(),
  is_enabled: z.boolean().optional(),
  thumb: z.string().optional(),
});

export type UpdateWinnerSchema = z.infer<typeof updateWinnerSchema>;
