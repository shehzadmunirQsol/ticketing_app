import { router, publicProcedure } from '../trpc';
import { TRPCError } from '@trpc/server';
import { addToCartSchema, getCartSchema } from '~/schema/cart';
import { prisma } from '~/server/prisma';

export const cartRouter = router({
  get: publicProcedure.input(getCartSchema).query(async ({ input }) => {
    try {
      const cart = await prisma.cart.findFirst({
        where: { customer_id: input.customer_id, is_deleted: false },
        include: { CartItems: true },
      });

      if (!cart) {
        throw new TRPCError({
          code: 'BAD_REQUEST',
          message: 'Invalid cart data',
        });
      }

      return { message: 'Cart found', data: cart };
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
        });

        if (!cartItem) {
          throw new TRPCError({
            code: 'BAD_REQUEST',
            message: 'Invalid cart item data',
          });
        }

        return { message: 'Cart added successfully!' };
      } catch (error: any) {
        throw new TRPCError({
          code: 'INTERNAL_SERVER_ERROR',
          message: error?.message,
        });
      }
    }),
});
