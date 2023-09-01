import { router, publicProcedure } from '../trpc';
import { TRPCError } from '@trpc/server';
import { createCheckoutSchema } from '~/schema/order';
import { prisma } from '~/server/prisma';

export const orderRouter = router({
  checkout: publicProcedure
    .input(createCheckoutSchema)
    .mutation(async ({ input }) => {
      try {
        const cart = await prisma.cart.findUnique({
          where: { id: input.cart_id },
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
                    id: true,
                    price: true,
                    tickets_sold: true,
                  },
                },
              },
            },
          },
        });

        if (!cart?.CartItems?.length) {
          throw new TRPCError({
            code: 'BAD_REQUEST',
            message: 'No items in the cart!',
          });
        }

        const isDiscount = cart?.CouponApply?.length ? true : false;
        const discount = cart?.CouponApply[0]?.discount ?? 0;
        const isPercentage = cart?.CouponApply[0]?.is_percentage ?? false;

        const subTotalAmount =
          cart?.CartItems.reduce(
            (accumulator, current) =>
              accumulator + current.quantity * current.Event.price,
            0,
          ) ?? 0;

        const discountAmount = isDiscount
          ? isPercentage
            ? subTotalAmount * (discount / 100)
            : discount
          : 0;

        const totalPaymentId = '0'; // from total payment gateway

        const orderPayload: any = {
          ...input,
          phone_number: input?.code + input?.phone_number,

          sub_total_amount: subTotalAmount,
          discount_amount: discountAmount,
          total_amount: subTotalAmount - discountAmount,
          total_payment_id: totalPaymentId,
          postal_code: '0',
        };
        if (input?.code) delete orderPayload?.code;
        if (input?.cart_id) delete orderPayload?.cart_id;

        const orderEventPayload = cart?.CartItems.map((item) => ({
          event_id: item.Event.id,
          ticket_price: item.Event.price,
          quantity: item.quantity,
          is_subscribe: item.is_subscribe,
        }));

        const orderSubscriptionPayload = cart?.CartItems.filter(
          (item) => item.is_subscribe,
        ).map((item) => {
          // logic for payment schedule
          const totalSubscriptionId = 'totalSubscriptionId';

          return {
            event_id: item.Event.id,
            ticket_price: item.Event.price,
            quantity: item.quantity,
            customer_id: cart.customer_id,
            total_subscription_id: totalSubscriptionId,
            subscription_type: item.subscription_type,
          };
        });

        await prisma.order.create({
          data: {
            ...orderPayload,
            OrderSubscription: {
              createMany: {
                data: orderSubscriptionPayload,
              },
            },
            OrderEvent: {
              createMany: {
                data: orderEventPayload,
              },
            },
          },
        });

        const eventPromises = cart.CartItems.map((item) =>
          prisma.event.update({
            where: { id: item.Event.id },
            data: {
              tickets_sold: (item?.Event?.tickets_sold ?? 0) + item?.quantity,
            },
          }),
        );

        await Promise.all(eventPromises);

        await prisma.cart.update({
          where: { id: cart.id },
          data: { is_deleted: true },
        });

        return { message: 'Order created successfully!' };
      } catch (error: any) {
        throw new TRPCError({
          code: 'INTERNAL_SERVER_ERROR',
          message: error?.message,
        });
      }
    }),
});
