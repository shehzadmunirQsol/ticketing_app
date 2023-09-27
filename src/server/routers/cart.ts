import { router, publicProcedure } from '../trpc';
import { TRPCError } from '@trpc/server';
import {
  addToCartSchema,
  getCartSchema,
  getTicketPurchasedSchema,
  removeCartItemSchema,
  getCartItemsSchema,
  createCartSchema,
} from '~/schema/cart';
import { prisma } from '~/server/prisma';
import { verifyJWT } from '~/utils/jwt';

const cartInclude = {
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
};

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
        include: cartInclude,
      });

      return { message: 'Cart found', data: cart };
    } catch (error: any) {
      throw new TRPCError({
        code: 'INTERNAL_SERVER_ERROR',
        message: error?.message,
      });
    }
  }),
  getCartItems: publicProcedure
    .input(getCartItemsSchema)
    .query(async ({ input }) => {
      try {
        const { filters, ...inputData } = input;
        const filterPayload: any = { ...filters };

        if (filterPayload?.searchQuery) delete filterPayload.searchQuery;
        if (filterPayload?.endDate) delete filterPayload.endDate;
        if (filterPayload?.startDate) delete filterPayload.startDate;
        const payload = {
          is_deleted: false,
          ...filterPayload,
        };

        if (input?.filters?.searchQuery) {
          payload.OR = [];
          payload.OR.push({
            Event: {
              EventDescription: {
                some: {
                  name: {
                    contains: input?.filters?.searchQuery,
                    mode: 'insensitive',
                  },
                },
              },
            },
          });
          payload.OR.push({
            Cart: {
              Customer: {
                first_name: {
                  contains: input?.filters?.searchQuery,
                  mode: 'insensitive',
                },
              },
            },
          });
          payload.OR.push({
            Cart: {
              Customer: {
                last_name: {
                  contains: input?.filters?.searchQuery,
                  mode: 'insensitive',
                },
              },
            },
          });
          payload.OR.push({
            Cart: {
              Customer: {
                email: {
                  contains: input?.filters?.searchQuery,
                  mode: 'insensitive',
                },
              },
            },
          });

          // options.where.OR.push({
          //   price: { contains: input.searchQuery, mode: 'insensitive' },
          // });
        }
        if (input?.filters?.startDate && !input?.filters?.endDate) {
          const startDate = (new Date(input?.filters?.startDate))?.toISOString().split("T")[0] as string;
          payload.created_at = { gte: new Date(startDate) };
        }
        if (input?.filters?.endDate && !input?.filters?.startDate) {
          const endDate = (new Date(input?.filters?.endDate))?.toISOString().split("T")[0] as string;
          payload.created_at = { lte: new Date(endDate) };
        }
        if (input?.filters?.endDate && input?.filters?.startDate) {
          const startDate = (new Date(input?.filters?.startDate))?.toISOString().split("T")[0] as string;
          const endDate = (new Date(input?.filters?.endDate))?.toISOString().split("T")[0] as string;
          payload.created_at = { gte: new Date(startDate), lte: new Date(endDate) };
        }
  

        const totalItemsPromise = prisma.cartItem.count({
          where: payload,
        });

        const cartItemsPromise = prisma.cartItem.findMany({
          orderBy: { created_at: 'desc' },
          skip: input.rows * input.first,
          take: input.rows,
          where: payload,
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

        const [totalItems, cartItems] = await Promise.all([
          totalItemsPromise,
          cartItemsPromise,
        ]);

        if (!cartItems?.length) {
          throw new TRPCError({
            code: 'NOT_FOUND',
            message: 'Cart Items not found',
          });
        }

        return { message: 'Cart found', data: cartItems, count: totalItems };
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

  createCart: publicProcedure
    .input(createCartSchema)
    .mutation(async ({ input }) => {
      try {
        const { customer_id, cart_id, cart_items: cartItems } = input;

        if (cart_id > 0) {
          const prevCart = await prisma.cart.findUnique({
            where: { id: cart_id },
            include: cartInclude,
          });

          if (!prevCart) {
            throw new TRPCError({
              code: 'BAD_REQUEST',
              message: 'Invalid cart data',
            });
          }

          const eventIds: number[] = cartItems.map((item) => item.event_id);
          const existingEventIds: number[] = [];

          prevCart?.CartItems.forEach((item) => {
            if (eventIds.includes(item.event_id))
              existingEventIds.push(item.event_id);
          });

          if (existingEventIds.length) {
            await prisma.cartItem.deleteMany({
              where: {
                cart_id: prevCart.id,
                event_id: {
                  in: existingEventIds,
                },
              },
            });
          }

          await prisma.cartItem.createMany({
            data: cartItems.map((item) => ({ ...item, cart_id })),
          });

          const cart = await prisma.cart.findUnique({
            where: { id: cart_id },
            include: cartInclude,
          });

          return { message: 'Cart created', data: cart };
        } else {
          const cart = await prisma.cart.create({
            data: {
              customer_id,
              CartItems: {
                createMany: {
                  data: cartItems,
                },
              },
            },
            include: cartInclude,
          });

          return { message: 'Cart created', data: cart };
        }
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
