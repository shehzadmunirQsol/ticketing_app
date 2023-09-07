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
  date: z.date().optional(),
  type: z.string(),
});

export const getClosingSoon = z.object({
  startDate: z.date().optional(),
  endDate: z.date().optional(),
  category_id: z.number().optional(),
  event_id: z.number().optional(),
  first: z.number(),
  rows: z.number(),
  lang_id: z.number(),
  launc_date: z.date().optional(),
  type: z.string(),
});

export const getFeatured = z.object({
  startDate: z.date().optional(),
  endDate: z.date().optional(),
  is_featured: z.number().optional(),
  event_id: z.number().optional(),
  first: z.number(),
  rows: z.number(),
  lang_id: z.number(),
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
  user_id: z.number(),
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

export const EventFormSchema = z.object({
  thumb: z.string(),
  multi_image: z.array(z.string()),
  price: z.number(),
  category_id: z.number(),
  video_src: z.string(),
  total_tickets: z.number(),
  cash_alt: z.number().nullable(),
  user_ticket_limit: z.number(),
  is_cash_alt: z.boolean().default(false),
  launch_date: z.date(),
  end_date: z.date(),

  en: z.object({
    name: z.string(),
    desc: z.string().optional(),
    comp_details: z.string().optional(),
  }),
  ar: z.object({
    name: z.string(),
    desc: z.string().optional(),
    comp_details: z.string().optional(),
  }),
});
export const enFormSchema = z.object({
  thumb: z.any(),
  link: z.string(),

  en: z.object({
    name: z.string(),
    description: z.string().optional(),
  }),
  ar: z
    .object({
      name: z.string(),
      description: z.string().optional(),
    })
    .optional(),
});
export const arFormSchema = z.object({
  thumb: z.any(),
  link: z.string(),
  en: z
    .object({
      name: z.string(),
      description: z.string().optional(),
    })
    .optional(),
  ar: z.object({
    name: z.string(),
    description: z.string().optional(),
  }),
});

export type GetEventSchema = z.infer<typeof getEventSchema>;

export const getEventsByIdSchema = z.object({
  id: z.number(),
});
