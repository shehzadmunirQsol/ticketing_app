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
  first_name: z
    .string()
    .min(2, {
      message: 'Name must be at least 2 characters',
    })
    .max(24, {
      message: 'Name must not exceed 24 characters',
    }),
  last_name: z
    .string()
    .min(2, {
      message: 'Name must be at least 2 characters',
    })
    .max(24, {
      message: 'Name must not exceed 24 characters',
    }),
  street_address: z.string(),
  apartment: z.string().optional(),
  country: z.string(),
  state: z.string(),
  city: z.string(),
  postal_code: z.string(),
  email: z.string().email(),
  code: z.string(),
  number: z.string(),
  dob: z.date(),
});

export type checkoutSchemaInput = z.infer<typeof createCheckoutSchema>;
