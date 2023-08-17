import { z } from 'zod';

export const getEventSchema = z.object({
  startDate: z.date().nullable(),
  endDate: z.date().nullable(),
  event_id: z.string(),
  searchQuery: z.string(),
  page: z.number(),
  first: z.number(),
  rows: z.number(),
});

export const createEventSchema = z.object({
  thumb: z.string(),
  video_src: z.string().optional(),
  price: z.number(),
  is_alt: z.boolean(),
  cash_alt: z.number().optional(),
  total_tickets: z.number(),
  user_ticket_limit: z.number(),
  tickets_sold: z.number(),
  launch_date: z.date().optional(),
  end_date: z.date().optional(),
  creator_id: z.number(),
  category_id: z.number(),

  en: z.object({
    name: z.string(),
    desc: z.string().optional(),
    comp_details: z.string().optional(),
    lang_id: z.number(),
  }),
  ar: z.object({
    name: z.string(),
    desc: z.string().optional(),
    comp_details: z.string().optional(),
    lang_id: z.number(),
  }),
});

export const deleteEventSchema = z.object({
  id: z.number(),
});
