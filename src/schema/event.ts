import { z } from 'zod';

export const getEventSchema = z.object({
  startDate: z.date().optional(),
  endDate: z.date().optional(),
  searchQuery: z.string().optional(),
  category_id: z.number().optional(),
  event_id: z.number().optional(),
  first: z.number(),
  rows: z.number(),
  lang_id: z.number(),
});
export const getUpcoming = z.object({
  startDate: z.date().optional(),
  endDate: z.date().optional(),
  category_id: z.number().optional(),
  event_id: z.number().optional(),
  first: z.number().optional(),
  rows: z.number().optional(),
  lang_id: z.number(),
  date: z.date(),
});

export const getClosingSoon = z.object({
  startDate: z.date().optional(),
  endDate: z.date().optional(),
  category_id: z.number().optional(),
  event_id: z.number().optional(),
  first: z.number().optional(),
  rows: z.number().optional(),
  lang_id: z.number(),
  date: z.date(),
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
