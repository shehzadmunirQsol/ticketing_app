import { z } from 'zod';

export const contactUsSchema = z.object({
  name: z.string(),
  email: z.string().email(),
  code: z.string(),
  number: z.string(),
  message: z.string(),
});

export const contactSchema = z.object({
  name: z
    .string()
    .min(2, {
      message: 'Name must be at least 2 characters',
    })
    .max(24, {
      message: 'Name must not exceed 24 characters',
    }),
  email: z.string().email(),
  code: z.string(),
  number: z
    .string()
    .min(9, {
      message: 'Number must be at least 9 characters',
    })
    .max(9, {
      message: 'Number must not exceed 9 characters',
    }),
  message: z.string(),
});

export type contactSchemaInput = z.infer<typeof contactSchema>;
