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
  filters: z.any().optional(),
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

export const deleteEventSchema = z.object({
  id: z.number(),
});

export const EventFormSchema = z.object({
  event_id: z.number().optional(),
  faq_id: z.number().optional(),
  removed_images: z.array(z.number()).optional(),
  thumb: z.string(),
  multi_image: z.array(z.string()),
  price: z.number({
    invalid_type_error: 'Please enter a price',
    required_error: 'Please enter a price',
  }),

  video_src: z.string(),
  cash_alt: z
    .number({
      required_error: 'Please enter a price',
      invalid_type_error: 'Please enter a valid price',
    })
    .nullable()
    .optional(),
  user_ticket_limit: z.number({
    required_error: 'Please enter user tickets limit',
    invalid_type_error: 'Please enter a valid limit',
  }),
  is_cash_alt: z.boolean().default(false),
  launch_date: z.date({
    invalid_type_error: 'Please select a valid date',
    required_error: 'Please select a date',
  }),
  end_date: z.date({
    invalid_type_error: 'Please select a valid date',
    required_error: 'Please select a date',
  }),
  category_id: z.number({
    required_error: 'Select a category',
  }),
  total_tickets: z.number({
    required_error: 'Please enter total tickets limit',
    invalid_type_error: 'Please enter a valid limit',
  }),

  en: z.object({
    name: z.string({
      required_error: 'Please enter a event name',
    }),
    desc: z.string({
      required_error: 'Please enter a description',
    }),
    comp_details: z.string({
      required_error: 'Please enter the competition details',
    }),
  }),
  ar: z.object({
    name: z.string({
      required_error: 'Please enter a event name',
    }),
    desc: z
      .string({
        required_error: 'Please enter a description',
      })
      .optional(),
    comp_details: z
      .string({
        required_error: 'Please enter the competition details',
      })
      .optional(),
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

export const getEventsByIdSchema = z.object({
  id: z.number(),
  lang_id: z.number().optional(),
  type: z.enum(['admin', 'client']).optional(),
});
export const getEventCustomers = z.object({
  event_id: z.number(),
});

export type GetEventSchema = z.infer<typeof getEventSchema>;
