import { z } from 'zod';

export const createFormPaymentSchema = z.object({
  card: z.object({
    number: z.string(),
    holder: z.string(),
    expiryMonth: z.string(),
    expiryYear: z.string(),
    cvv: z.string(),
  }),
  paymentBrand: z.string(),
  registrationId: z.string().optional(),
  price: z.number().optional(),
  customer_id: z.number().optional(),

  cart: z.any(),
});
export const createPaymentSchema = z.object({
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
  price: z.number().optional(),
  customer_id: z.number().optional(),
  cart: z.array(z.any()).optional(),
});
export type createPaymentSchema = z.infer<typeof createPaymentSchema>;
export type createFormPaymentSchema = z.infer<typeof createFormPaymentSchema>;
