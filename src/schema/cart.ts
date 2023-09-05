import { z } from 'zod';

export const getCartSchema = z.object({
  customer_id: z.number(),
});

export const getTicketPurchasedSchema = z.object({
  event_ids: z.array(z.number()),
});

export const removeCartItemSchema = z.object({
  cart_item_id: z.number(),
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
