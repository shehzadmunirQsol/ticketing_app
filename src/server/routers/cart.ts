import { router, publicProcedure } from '../trpc';
import { TRPCError } from '@trpc/server';
import {
  addToCartSchema,
  getCartSchema,
  getTicketPurchasedSchema,
  removeCartItemSchema,
} from '~/schema/cart';
import { prisma } from '~/server/prisma';
import { verifyJWT } from '~/utils/jwt';

export const cartRouter = router({
  get: publicProcedure.input(getCartSchema).query(async ({ ctx }) => {
    try {
      const token = ctx?.req?.cookies['winnar-token'];

      let userData;
      if (token) {
        userData = await verifyJWT(token);
      } else {
        return { data: null };
      }

      const cart = await prisma.cart.findFirst({
        where: { customer_id: userData.id, is_deleted: false },
        include: {
          CouponApply: {
            where: { is_deleted: false },
            select: {
              discount: true,
              is_percentage: true,
            },
          },
          CartItems: {
            include: {
              Event: {
                select: {
                  thumb: true,
                  price: true,
                  end_date: true,
                  tickets_sold: true,
                  user_ticket_limit: true,
                  total_tickets: true,

                  EventDescription: {
                    where: { lang_id: 1 },
                    select: {
                      name: true,
                    },
                  },
                },
              },
            },
          },
        },
      });

      return { message: 'Cart found', data: cart };
    } catch (error: any) {
      throw new TRPCError({
        code: 'INTERNAL_SERVER_ERROR',
        message: error?.message,
      });
    }
  }),
  getCartItems: publicProcedure.query(async ({ ctx }) => {
    try {
      const cartItems = await prisma.cartItem.findMany({
        orderBy: { created_at: 'desc' },
        take: 10,
        where: { is_deleted: false },
        select: {
          id: true,
          created_at: true,
          updated_at: true,
          quantity: true,
          is_subscribe: true,
          subscription_type: true,
          Event: {
            select: {
              id: true,
              thumb: true,
              price: true,
              EventDescription: {
                where: { lang_id: 1 },
                select: {
                  name: true,
                },
              },
            },
          },
          Cart: {
            select: {
              Customer: {
                select: {
                  id: true,
                  email: true,
                  first_name: true,
                },
              },
            },
          },
        },
      });

      return { message: 'Cart found', data: cartItems };
    } catch (error: any) {
      throw new TRPCError({
        code: 'INTERNAL_SERVER_ERROR',
        message: error?.message,
      });
    }
  }),
  getUserTicketLimit: publicProcedure
    .input(getTicketPurchasedSchema)
    .query(async ({ input, ctx }) => {
      try {
        const token = ctx?.req?.cookies['winnar-token'];

        let userData;
        if (token) {
          userData = await verifyJWT(token);
        } else {
          return { data: null };
        }
        const customerLimit = await prisma.orderEvent.groupBy({
          having: {
            customer_id: userData?.id,
            event_id: { in: input.event_ids },
          },
          by: ['event_id', 'customer_id'],
          _sum: { quantity: true },
        });

        return { message: 'Found', data: customerLimit };
      } catch (error: any) {
        throw new TRPCError({
          code: 'INTERNAL_SERVER_ERROR',
          message: error?.message,
        });
      }
    }),
  addToCart: publicProcedure
    .input(addToCartSchema)
    .mutation(async ({ input }) => {
      try {
        const { cart_item_id, customer_id, ...cartItemPayload } = input;

        if (input.cart_id > 0) {
          const cart = await prisma.cart.findUnique({
            where: { id: input.cart_id },
          });

          if (!cart) {
            throw new TRPCError({
              code: 'BAD_REQUEST',
              message: 'Invalid cart data',
            });
          }
          cartItemPayload.cart_id = cart.id;
        } else {
          const cart = await prisma.cart.create({ data: { customer_id } });

          cartItemPayload.cart_id = cart.id;
        }

        const cartItem = await prisma.cartItem.upsert({
          create: cartItemPayload,
          update: cartItemPayload,
          where: { id: cart_item_id },
          include: {
            Event: {
              select: {
                thumb: true,
                price: true,
                end_date: true,
                tickets_sold: true,
                user_ticket_limit: true,
                total_tickets: true,
                EventDescription: {
                  where: { lang_id: 1 },
                  select: {
                    name: true,
                  },
                },
              },
            },
          },
        });

        if (!cartItem) {
          throw new TRPCError({
            code: 'BAD_REQUEST',
            message: 'Invalid cart item data',
          });
        }

        const cartResponse = {
          id: cartItemPayload.cart_id,
          customer_id: input.customer_id,
          cartItem: cartItem,
        };

        return { message: 'Cart added successfully!', data: cartResponse };
      } catch (error: any) {
        throw new TRPCError({
          code: 'INTERNAL_SERVER_ERROR',
          message: error?.message,
        });
      }
    }),
  removeFromCart: publicProcedure
    .input(removeCartItemSchema)
    .mutation(async ({ input }) => {
      try {
        await prisma.cartItem.delete({ where: { id: input.cart_item_id } });

        return { message: 'Item removed' };
      } catch (error: any) {
        throw new TRPCError({
          code: 'INTERNAL_SERVER_ERROR',
          message: error?.message,
        });
      }
    }),
});
