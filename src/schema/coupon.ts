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
  category_id: z.number().optional(),
  first: z.number(),
  rows: z.number(),
});
