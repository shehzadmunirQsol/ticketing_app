import { z } from 'zod';

export const cmsSchema = z.object({
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
export type cmsSchemaInput = z.infer<typeof cmsSchemaInput>;

export const getCmsSchema = z.object({});
export const getCmsContentByIdSchema = z.object({
  id: z.number(),
});

export const updateCmsContentById = z.object({
  id: z.number(),
  content: z.string(),
  slug: z.string(),
  type: z.string(),
  en: z.object({
    slug: z.string(),
    title: z.string(),
    metatitle: z.string(),
    metadesc: z.string(),
    desc: z.string(),
  }),
});
