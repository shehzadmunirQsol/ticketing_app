import { z } from 'zod';

export const cmsSchema = z.object({
  slug: z.string(),
  type: z.enum(['event_faqs', 'faqs', 'static']),
  en: z.object({
    title: z.string(),
    meta_keywords: z.string(),
    desc: z.string(),
    content: z.string(),
  }),
  ar: z.object({
    title: z.string(),
    meta_keywords: z.string(),
    desc: z.string(),
    content: z.string(),
  }),
});
export const cmsSchemaInput = z.object({
  content: z.string().optional(),
  slug: z.string().optional(),
  type: z.enum(['event_faqs', 'faqs', 'static']).nullable(),
  en: z.object({
    slug: z.string(),
    title: z.string(),
    metatitle: z.string(),
    metadesc: z.string(),
    desc: z.string(),
  }),
});
export type cmsSchemaForm = z.infer<typeof cmsSchema>;

export const getCmsSchema = z.object({});
export const getCmsContentByIdSchema = z.object({
  id: z.number(),
});

export const updateCmsContentById = z.object({
  id: z.number(),
  slug: z.string(),
  type: z.enum(['event_faqs', 'faqs', 'static']),

  en: z.object({
    title: z.string(),
    meta_keywords: z.string(),
    desc: z.string(),
    content: z.string(),
  }),
  ar: z.object({
    title: z.string(),
    meta_keywords: z.string(),
    desc: z.string(),
    content: z.string(),
  }),
});

export const cmsStatusUpdateById = z.object({
  id: z.number(),
});
