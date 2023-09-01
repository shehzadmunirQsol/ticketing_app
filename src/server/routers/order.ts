import { router, publicProcedure } from '../trpc';
import { TRPCError } from '@trpc/server';
import {
  createCheckoutPaymentSchema,
  createCheckoutSchema,
} from '~/schema/order';
import https from 'https';

import { prisma } from '~/server/prisma';

export const orderRouter = router({
  checkout: publicProcedure
    .input(createCheckoutPaymentSchema)
    .mutation(async ({ input }) => {
      try {
        const cart = await prisma.cart.findUnique({
          where: { id: input?.values?.cart_id },
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
        const paymentPayload: any = {
          ...input,
          sub_total_amount: subTotalAmount,
          discount_amount: discountAmount,
          total_amount: subTotalAmount - discountAmount,
          cartItem: cart?.CartItems,
        };
        if (input?.values) delete paymentPayload?.values;
        const apiRes: any = await CreatePayment({
          ...paymentPayload,
        })
          .then((response: any) => {
            if (!response?.result?.parameterErrors) {
              return { data: response, success: true };
            }
            throw new Error(response?.result?.parameterErrors[0].message);
          })
          .catch((error) => {
            throw new Error(error.message);
          });
        console.log(apiRes, 'apiRes?.registrationId');
        let user;
        if (!input?.registrationId) {
          user = await prisma.customer?.update({
            where: {
              id: input?.customer_id,
            },
            data: {
              total_customer_id: apiRes?.data?.registrationId as string,
            },
          });
        }

        const totalPaymentId = apiRes?.data?.id; // from total payment gateway

        const orderPayload: any = {
          ...input?.values,
          phone_number: input?.values?.code + input?.values?.phone_number,

          sub_total_amount: subTotalAmount,
          discount_amount: discountAmount,
          total_amount: subTotalAmount - discountAmount,
          total_payment_id: totalPaymentId,
          postal_code: '0',
        };
        if (input?.values?.code) delete orderPayload?.code;
        if (input?.values?.cart_id) delete orderPayload?.cart_id;

        const orderEventPayload = cart?.CartItems.map((item) => ({
          event_id: item.Event.id,
          ticket_price: item.Event.price,
          quantity: item.quantity,
          is_subscribe: item.is_subscribe,
        }));

        const orderSubscriptionPayload = cart?.CartItems.filter(
          (item) => item.is_subscribe,
        ).map(async (item) => {
          // logic for payment schedule
          const paymentPayload: any = {
            ...input,
            sub_total_amount: subTotalAmount,
            discount_amount: discountAmount,
            total_amount: subTotalAmount - discountAmount,
            cartItem: { ...item },
          };
          if (input?.values) delete paymentPayload?.values;
          const apiRes: any = await CreatePayment({
            ...paymentPayload,
          })
            .then((response: any) => {
              if (!response?.result?.parameterErrors) {
                return { data: response, success: true };
              }
              throw new Error(response?.result?.parameterErrors[0].message);
            })
            .catch((error) => {
              throw new Error(error.message);
            });
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

        return { message: 'Order created successfully!', user: user };
      } catch (error: any) {
        throw new TRPCError({
          code: 'INTERNAL_SERVER_ERROR',
          message: error?.message,
        });
      }
    }),
});

async function CreatePayment(APidata: any) {
  try {
    const path = APidata?.registrationId
      ? `/v1/registrations/${APidata?.registrationId}/payments`
      : '/v1/payments';
    const payload = { ...APidata };
    if (payload?.card) delete payload?.card;
    const apiDate: any = APidata?.registrationId
      ? {
          entityId: process.env.TOTAN_ENTITY_ID,
          amount: +APidata?.total_amount,
          currency: 'AED',
          paymentType: 'DB',
          'standingInstruction.source': 'CIT',
          // wpwlOptions: JSON.stringify(APidata?.cart),
          'customParameters[payload]': JSON.stringify({
            ...payload,
          }),

          'standingInstruction.type': 'UNSCHEDULED',
        }
      : {
          entityId: process.env.TOTAN_ENTITY_ID,
          amount: APidata?.total_amount,
          currency: 'AED',
          paymentBrand: APidata?.paymentBrand,
          paymentType: 'DB',

          'card.number':
            APidata?.card?.number && +APidata?.card?.number.replaceAll(' ', ''),
          'card.holder': APidata?.card?.holder && APidata?.card?.holder,
          'card.expiryMonth':
            APidata?.card?.expiryMonth && APidata?.card?.expiryMonth,
          'card.expiryYear':
            APidata?.card?.expiryYear && APidata?.card?.expiryYear,
          'card.cvv': APidata?.card?.cvv && +APidata?.card?.cvv,
          'standingInstruction.mode': 'INITIAL',
          'standingInstruction.source': 'CIT',
          'customParameters[payload]': JSON.stringify({
            ...payload,
          }),

          createRegistration: 'true',
        };

    const data = new URLSearchParams(apiDate).toString();
    const options = {
      port: 443,
      host: 'eu-test.oppwa.com',
      path: path,
      method: 'POST',
      headers: {
        'Content-Type': 'application/x-www-form-urlencoded',
        'Content-Length': data.length,
        Authorization: process.env.TOTAL_PROCESSING_BEARER,
      },
    };
    return new Promise((resolve, reject) => {
      const postRequest = https.request(options, function (res) {
        const buf: any = [];
        res.on('data', (chunk) => {
          buf.push(Buffer.from(chunk));
        });
        res.on('end', () => {
          const jsonString = Buffer.concat(buf).toString('utf8');
          try {
            resolve(JSON.parse(jsonString));
          } catch (error) {
            reject(error);
          }
        });
      });
      postRequest.on('error', reject);
      postRequest.write(data);
      postRequest.end();
    });
  } catch (error: any) {
    console.log({ error }, 'function error');
    throw new Error(error.message);
  }
}
async function CreateSubscription(APidata: any) {
  try {
    const path = '/scheduling/v1/schedules';
    const apiDate: any = {
      entityId: process.env.TOTAN_ENTITY_ID,
      amount: APidata?.total_amount,
      registrationId: APidata?.registrationId,
      currency: 'AED',
      paymentType: 'DB',
      'standingInstruction.type': 'RECURRING',
      'job.dayOfWeek': '1',
      'customParameters[payload]': JSON.stringify({
        ...APidata,
      }),
      'job.endDate': APidata?.end_date,
    };
    const data = new URLSearchParams(apiDate).toString();
    const options = {
      port: 443,
      host: 'eu-test.oppwa.com',
      path: path,
      method: 'POST',
      headers: {
        'Content-Type': 'application/x-www-form-urlencoded',
        'Content-Length': data.length,
        Authorization: process.env.TOTAL_PROCESSING_BEARER,
      },
    };
    return new Promise((resolve, reject) => {
      const postRequest = https.request(options, function (res) {
        const buf: any = [];
        res.on('data', (chunk) => {
          buf.push(Buffer.from(chunk));
        });
        res.on('end', () => {
          const jsonString = Buffer.concat(buf).toString('utf8');
          try {
            resolve(JSON.parse(jsonString));
          } catch (error) {
            reject(error);
          }
        });
      });
      console.log({ data }, 'data for test');
      postRequest.on('error', reject);
      postRequest.write(data);
      postRequest.end();
    });
  } catch (error: any) {
    console.log({ error }, 'function error');
    throw new Error(error.message);
  }
}
