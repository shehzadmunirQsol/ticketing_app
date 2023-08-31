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
  cart: z.any(),
});
export const createPaymentSchema = z.object({
  card: z
    .object({
      number: z.string(),
      holder: z.string(),
      expiryMonth: z.string(),
      expiryYear: z.string(),
      cvv: z.string(),
    })
    .optional(),
  paymentBrand: z.string(),
  registrationId: z.string().optional(),
  price: z.number().optional(),
  cart: z.any(),
});
export type createPaymentSchema = z.infer<typeof createPaymentSchema>;
