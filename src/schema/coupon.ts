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
  filters: z.any().optional(),
  is_enabled:z.boolean().optional(),
});

export const createCouponSchema = z.object({
  user_id: z.number(),
  coupon_id: z.number().optional(),
  name: z.string({ required_error: ' Please enter a name' }).trim(),
  coupon_code: z
    .string({ required_error: ' Please enter a coupon code' })
    .min(6, {
      message: 'Coupon Code must be 6 characters',
    })
    .max(6, {
      message: 'Coupon Code must be 6 characters',
    }).trim().refine(s => !s.includes(' '), `Please don't use spaces`),
  is_percentage: z.string({ required_error: ' Please select a discount type' }),
  is_limited: z.string(),
  coupon_limit: z.number().optional(),
  discount: z.number({ invalid_type_error: ' Please enter a discount' }),
  start_date: z.date({
    invalid_type_error: 'Please select a start date',
    required_error: 'Please select a start date',
  }),
  end_date: z.date({
    invalid_type_error: 'Please select a start date',
    required_error: 'Please select a start date',
  }),
});
export const updateSchema = z.object({
  user_id: z.number(),
  coupon_id: z.number().optional(),
  name: z.string().trim(),
  coupon_code: z
    .string()
    .max(6, {
      message: 'Coupon Code must be at least 6 characters',
    })
    .max(6, {
      message: 'Coupon Code must be at least 6 characters',
    }).trim().refine(s => !s.includes(' '), `Please don't use spaces`),
  is_percentage: z.string(),
  is_limited: z.string(),
  coupon_limit: z.number().optional(),
  discount: z.number(),
});
export const updateCouponSchema = z.object({
  coupon_id: z.number(),
  is_enabled: z.boolean().optional(),
});

export type createCouponSchema = z.infer<typeof createCouponSchema>;
export type updateCouponType = z.infer<typeof updateCouponSchema>;
