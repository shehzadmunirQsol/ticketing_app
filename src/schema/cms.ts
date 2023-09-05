import { z } from 'zod';

export const cmsSchema = z.object({
  en: z.object({
    title: z.string(),
    slug: z.string(),
    metatitle: z.string(),
    metadesc: z.string(),
    desc: z.string(),
    content: z.string(),
  }),

});

export const getCmsSchema = z.object({});
export const getCmsContentByIdSchema = z.object({
    id: z.number(),
  });
