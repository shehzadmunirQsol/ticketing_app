import { z } from 'zod';

export const checkoutSchema = z.object({
  firstname: z.string(),
  lastname: z.string(),
  email: z.string().email(),
  dateofbirth: z.string(),
  number: z.string(),
  code: z.string(),
  postcode: z.string(),
  state: z.string(),
  city: z.string(),
  street: z.string(),
  country: z.string(),
  apartment: z.string(),
});

export const createCheckoutSchema = z.object({
    firstname: z
      .string()
      .min(2, {
        message: 'Name must be at least 2 characters',
      })
      .max(24, {
        message: 'Name must not exceed 24 characters',
      }),
      lastname: z.string().optional(),
      street: z.string(),
      apartment: z.string().optional(),
      country: z.string(),
      state: z.string(),
      city: z.string(),
      postcode: z.string(),
      email: z.string().email(),
      code: z.string(),
      number: z.string(),
      dateofbirth: z.string(),
});

export type checkoutSchemaInput = z.infer<typeof createCheckoutSchema>;
