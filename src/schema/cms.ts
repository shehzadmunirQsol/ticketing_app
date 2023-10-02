import { z } from 'zod';

export const cmsSchema = z.object({
  slug: z.string({ required_error: 'Please enter your slug' }).min(1, {
    message: 'Please enter your slug',
  }).trim(),
  type: z.enum(['event_faqs', 'faqs', 'static'], {
    required_error: 'Please enter your CMS type',
  }),
  en: z.object({
    title: z.string({ required_error: 'Please enter your title' }).min(1, {
      message: 'Please enter your title',
    }).trim(),
    meta_keywords: z
      .string({ required_error: 'Please enter your meta keywords' })
      .min(1, {
        message: 'Please enter your meta keywords',
      }).trim(),
    desc: z.string({ required_error: 'Please enter your description' }).min(1, {
      message: 'Please enter your description',
    }).trim(),
    content: z.string({ required_error: 'Please enter your content' }).min(1, {
      message: 'Please enter your content',
    }).trim(),
  }),
  ar: z.object({
    title: z.string({ required_error: 'Please enter your title' }).min(1, {
      message: 'Please enter your title',
    }).trim(),
    meta_keywords: z
      .string({ required_error: 'Please enter your meta keywords' })
      .min(1, {
        message: 'Please enter your meta keywords',
      }).trim(),
    desc: z.string({ required_error: 'Please enter your description' }).min(1, {
      message: 'Please enter your description',
    }).trim(),
    content: z.string({ required_error: 'Please enter your content' }).min(1, {
      message: 'Please enter your content',
    }).trim(),
  }),
});
export const cmsSchemaInput = z.object({
  content: z.string().optional(),
  slug: z.string().trim().optional(),
  type: z.enum(['event_faqs', 'faqs', 'static']).nullable(),
  en: z.object({
    slug: z.string().trim(),
    title: z.string().trim(),
    metatitle: z.string().trim(),
    metadesc: z.string().trim(),
    desc: z.string().trim(),
  }),
});
export type cmsSchemaForm = z.infer<typeof cmsSchema>;

export const getCmsSchema = z.object({});
export const getCmsContentByIdSchema = z.object({
  id: z.number(),
});

export const updateCmsContentById = z.object({
  id: z.number(),
  slug: z.string().trim(),
  type: z.enum(['event_faqs', 'faqs', 'static']),

  en: z.object({
    title: z.string().trim(),
    meta_keywords: z.string().trim(),
    desc: z.string().trim(),
    content: z.string().trim(),
  }),
  ar: z.object({
    title: z.string().trim(),
    meta_keywords: z.string().trim(),
    desc: z.string().trim(),
    content: z.string().trim(),
  }),
});

export const cmsStatusUpdateById = z.object({
  id: z.number(),
});
