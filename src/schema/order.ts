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

export const getCheckoutIDSchema = z.object({
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
    total_id: z.string().optional().nullable(),
  }),
});

export const getPaymentStatusSchema = z.object({
  checkout_id: z.string(),
});
export const getOrder = z.object({
  startDate: z.date().optional(),
  endDate: z.date().optional(),
  customer_id: z.number().optional(),
  first: z.number().optional(),
  rows: z.number().optional(),
  lang_id: z.number().optional(),
});

export const getOrderSchema = z.object({
  startDate: z.date().optional(),
  endDate: z.date().optional(),
  searchQuery: z.string().optional(),
  category_id: z.number().optional(),
  event_id: z.number().optional(),
  first: z.number(),
  rows: z.number(),
  lang_id: z.number().optional(),
  filters: z.any().optional(),
});
export const getByIDSchema = z.object({
  order_id: z.number(),
});
export type CreateCheckoutSchema = z.infer<typeof createCheckoutSchema>;
