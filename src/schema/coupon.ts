import { z } from 'zod';

export const applyCouponSchema = z.object({
  customer_id: z.number(),
  cart_id: z.number(),
  coupon_code: z.string(),
});

export const addToCartSchema = z.object({
  cart_id: z.number().default(0),
  cart_item_id: z.number().default(0),
  customer_id: z.number(),
  event_id: z.number(),
  quantity: z.number(),
  is_subscribe: z.boolean().default(false),
  subscription_type: z.enum(['weekly', 'monthly', 'quarterly']).nullable(),
});

export const getCouponSchema = z.object({
  startDate: z.date().optional(),
  endDate: z.date().optional(),
  searchQuery: z.string().optional(),
  first: z.number(),
  rows: z.number(),
});

export const createCouponSchema = z.object({
  user_id: z.number(),
  name: z.string(),
  coupon_code: z
    .string()
    .max(6, {
      message: 'Coupon Code must be at least 6 characters',
    })
    .max(6, {
      message: 'Coupon Code must be at least 6 characters',
    }),
  is_percentage: z.string(),
  is_limited: z.string(),
  limit: z.number().optional(),
  discount: z.number(),
  start_date: z.date(),
  end_date: z.date(),
});

export type createCouponSchema = z.infer<typeof createCouponSchema>;
