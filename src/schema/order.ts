import { z } from 'zod';

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
  phone_number: z.string(),
  dob: z.date(),
  cart_id: z.number(),
  customer_id: z.number(),
});
export const createCheckoutPaymentSchema = z.object({
  values: z.object({
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
    phone_number: z.string(),
    dob: z.date(),
    cart_id: z.number(),
    customer_id: z.number(),
  }),
  card: z
    .object({
      number: z.string().optional(),
      holder: z.string().optional(),
      expiryMonth: z.string().optional(),
      expiryYear: z.string().optional(),
      cvv: z.string().optional(),
    })
    .optional(),
  paymentBrand: z.string().optional(),
  registrationId: z.string().optional().nullable(),
  customer_id: z.number().optional(),
});


export const getOrder = z.object({
  id: z.number(),
  lang_id: z.number()
})
export type CreateCheckoutSchema = z.infer<typeof createCheckoutSchema>;
