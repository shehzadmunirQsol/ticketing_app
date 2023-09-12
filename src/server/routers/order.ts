import { router, publicProcedure } from '../trpc';
import { TRPCError } from '@trpc/server';
import {
  createCheckoutPaymentSchema,
  createCheckoutSchema,
  getOrder,
  getByIDSchema,
  getOrderSchema,
} from '~/schema/order';
import https from 'https';

import { prisma } from '~/server/prisma';
import { EMAIL_TEMPLATE_IDS, sendEmail } from '~/utils/helper';

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
                    end_date: true,
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

        const isDiscount = cart?.CouponApply?.length > 0;
        const discount = cart?.CouponApply[0]?.discount ?? 0;
        const isPercentage = cart?.CouponApply[0]?.is_percentage ?? false;

        const subTotalAmount =
          cart?.CartItems.reduce(
            (accumulator, current) =>
              accumulator + current.quantity * current.Event.price,
            0,
          ) ?? 0;

        const discountAmount =
          isDiscount && isPercentage
            ? subTotalAmount * (discount / 100)
            : discount;

        // Total Processing Initial Payment Process
        const paymentPayload: any = {
          ...input,
          sub_total_amount: subTotalAmount,
          discount_amount: discountAmount,
          total_amount: subTotalAmount - discountAmount,
          cartItem: cart?.CartItems,
        };
        const paymentRes: any = await CreatePayment({
          ...paymentPayload,
        })
          .then((response: any) => {
            console.log(
              response?.result?.parameterErrors,
              'response?.result?.parameterErrors',
            );
            if (!response?.result?.parameterErrors) {
              return { data: response, success: true };
            }
            throw new Error(response?.result?.parameterErrors[0].message);
          })
          .catch((error) => {
            console.log(
              error?.parameterErrors,
              'response?.result?.parameterErrors',
            );
            throw new Error(error.message);
          });
        console.log(paymentRes, 'apiRes?.registrationId');
        let user;
        if (!input?.registrationId) {
          user = await prisma.customer?.update({
            where: {
              id: input?.values?.customer_id,
            },
            data: {
              total_customer_id: paymentRes?.data?.registrationId as string,
            },
          });
        }

        const totalPaymentId = paymentRes?.data?.id; // from total payment gateway

        const orderPayload: any = {
          ...input?.values,
          phone_number: input?.values?.code + input?.values?.phone_number,

          sub_total_amount: subTotalAmount,
          discount_amount: discountAmount,
          total_amount: subTotalAmount - discountAmount,
          total_payment_id: totalPaymentId,
        };
        if (input?.values?.code) delete orderPayload?.code;
        if (input?.values?.cart_id) delete orderPayload?.cart_id;

        const orderEventPayload = cart?.CartItems.map((item) => ({
          event_id: item.Event.id,
          customer_id: input?.values?.customer_id,
          ticket_price: item.Event.price,
          quantity: item.quantity,
          is_subscribe: item.is_subscribe,
        }));

        const order = await prisma.order.create({
          data: {
            ...orderPayload,
            OrderEvent: {
              createMany: {
                data: orderEventPayload,
              },
            },
          },
        });

        const orderSubscriptionPayload = await Promise.all(
          cart?.CartItems.filter((item) => item.is_subscribe).map(
            async (item) => {
              // logic for payment schedule
              // Total Processing Subscription Process
              const subPayload: any = {
                ...input,
                registrationId: input?.registrationId
                  ? input?.registrationId
                  : paymentRes?.data?.registrationId,

                total_amount: +(+item.Event.price * +item.quantity),
                event_id: item.Event.id,
                ticket_price: item.Event.price,
                quantity: item.quantity,
                customer_id: input?.values?.customer_id,
                subscription_type: item.subscription_type,
                end_date: item.Event.end_date,
                cartItem: { ...item },
                order_id: order.id,
              };
              if (input?.values) delete subPayload?.values;
              const apiRes: any = await CreateSubscription({
                ...subPayload,
              })
                .then((response: any) => {
                  console.log(response?.result, 'response?.resultsdaasdasd');

                  if (!response?.result?.parameterErrors) {
                    return { data: response, success: true };
                  }
                  throw new Error(response?.result?.parameterErrors[0].message);
                })
                .catch((error) => {
                  throw new Error(error.message);
                });
              const totalSubscriptionId = apiRes?.data?.id;
              return {
                order_id: order.id,
                event_id: item.Event.id,
                ticket_price: item.Event.price,
                quantity: item.quantity,
                customer_id: input?.values?.customer_id,
                total_subscription_id: totalSubscriptionId,
                subscription_type: item.subscription_type,
              };
            },
          ),
        );

        await prisma.orderSubscription.createMany({
          data: orderSubscriptionPayload,
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

        const mailOptions = {
          template_id: EMAIL_TEMPLATE_IDS.ORDER_SUCCESS,
          from: 'no-reply@winnar.com',
          to: input.values.email,
          subject: 'Your order has been placed 🎉',
          params: {
            first_name: input.values.first_name,
            status: 'paid',
          },
        };

        await sendEmail(mailOptions);

        return { message: 'Order created successfully!', user: user };
      } catch (error: any) {
        console.log({ error }, 'error message');
        throw new TRPCError({
          code: 'INTERNAL_SERVER_ERROR',
          message: error?.message,
        });
      }
    }),

  getOrders: publicProcedure.input(getOrder).query(async ({ input, ctx }) => {
    try {
      const orders: any = await prisma.order.findMany({
        where: {
          customer_id: input?.customer_id,
        },
        include: {
          OrderEvent: {
            include: {
              Event: {
                include: {
                  EventDescription: {
                    where: {
                      lang_id: input.lang_id,
                    },
                  },
                },
              },
            },
          },
        },
      });

      console.log({ orders }, 'ordersorders');
      if (orders && orders?.length > 0) {
        const todayDate = new Date();

        const ret: any = { current: [], past: [] };

        for (let i = 0; i < orders.length; i++) {
          if (orders[i].OrderEvent[0].Event?.end_date < todayDate) {
            ret.past.push(orders[i]);
          } else {
            ret.current.push(orders[i]);
          }
        }

        return ret;
      } else {
        throw new TRPCError({
          code: 'NOT_FOUND',
          message: 'No Orders Found',
        });
      }
    } catch (error: any) {
      throw new TRPCError({
        code: 'INTERNAL_SERVER_ERROR',
        message: error?.message,
      });
    }
  }),
  get: publicProcedure.input(getOrderSchema).query(async ({ input }) => {
    try {
      const where: any = { is_deleted: false };

      if (input?.startDate) {
        const startDate = new Date(input?.startDate);
        where.created_at = { gte: startDate };
      }
      if (input?.endDate) {
        const endDate = new Date(input?.endDate);
        where.created_at = { lte: endDate };
      }

      const totalEventPromise = prisma.order.count({
        where: where,
      });

      const eventPromise = prisma.order.findMany({
        orderBy: { created_at: 'desc' },
        skip: input.first * input.rows,
        take: input.rows,
        where: where,
        include: {
          Customer: {
            select: {
              email: true,
              first_name: true,
              last_name: true,
            },
          },
        },
      });

      const [totalEvent, event] = await Promise.all([
        totalEventPromise,
        eventPromise,
      ]);

      if (!event?.length) {
        throw new TRPCError({
          code: 'NOT_FOUND',
          message: 'Events not found',
        });
      }

      return {
        message: 'events found',
        count: totalEvent,
        data: event,
      };
    } catch (error: any) {
      throw new TRPCError({
        code: 'INTERNAL_SERVER_ERROR',
        message: error?.message,
      });
    }
  }),
  getByID: publicProcedure.input(getByIDSchema).query(async ({ input }) => {
    try {
      const where: any = { is_deleted: false, id: input?.order_id };

      const eventPromise = prisma.order.findFirst({
        orderBy: { created_at: 'desc' },

        where: where,
        include: {
          Customer: {
            select: {
              email: true,
              first_name: true,
              last_name: true,
            },
          },
          OrderEvent: {
            include: {
              Event: {
                include: {
                  EventDescription: true,
                },
              },
            },
          },
        },
      });
      const [event] = await Promise.all([eventPromise]);
      if (!event) {
        throw new TRPCError({
          code: 'NOT_FOUND',
          message: 'Events not found',
        });
      }
      return {
        message: 'events found',
        data: event,
      };
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
    console.log(APidata, 'APidata?.paymentBrandsss');

    if (payload?.card) delete payload?.card;
    if (payload?.values) delete payload?.values;
    const tot_amount = APidata?.total_amount.toFixed(2);
    console.log(tot_amount, 'tot_amount');
    const apiDate: any = APidata?.registrationId
      ? {
          entityId: process.env.TOTAN_ENTITY_ID,
          amount: APidata?.total_amount.toFixed(2),
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
          amount: APidata?.total_amount.toFixed(2),
          currency: 'AED',
          paymentType: 'DB',
          paymentBrand: APidata?.paymentBrand,

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
          // createRegistration: 'true',
          'merchant.name': 'MerchantCo',
          'merchant.city': 'Munich',
          'merchant.country': 'DE',
          'merchant.mcc': '5399',
          'customer.ip': '192.168.0.1',
          'customer.browser.acceptHeader': 'text/html',
          'customer.browser.screenColorDepth': '48',
          'customer.browser.javaEnabled': 'false',
          'customer.browser.language': 'de',
          'customer.browser.screenHeight': '1200',
          'customer.browser.screenWidth': '1600',
          'customer.browser.timezone': '60',
          'customer.browser.challengeWindow': '4',
          'customer.browser.userAgent':
            'Mozilla/4.0 (MSIE 6.0; Windows NT 5.0)',
          testMode: 'EXTERNAL',

          'customParameters[payload]': JSON.stringify({
            ...payload,
          }),
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
            console.log(error, 'error error error error');
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
    const payload = { ...APidata };

    if (payload?.card) delete payload?.card;
    if (payload?.values) delete payload?.values;

    const path = '/scheduling/v1/schedules';
    const subType: any = {};
    if (APidata?.subscription_type == 'weekly') subType['job.dayOfWeek'] = '1';
    if (APidata?.subscription_type == 'monthly') {
      subType['job.month'] = '*';
      subType['job.dayOfMonth'] = '1';
    }
    if (APidata?.subscription_type == 'quarterly') {
      subType['job.month'] = '3';
      subType['job.dayOfMonth'] = '1';
    }
    if (APidata?.subscription_type == 'yearly') subType['job.year'] = '*';
    const payloadData: any = {
      entityId: process.env.TOTAN_ENTITY_ID,
      amount: APidata?.total_amount,
      registrationId: APidata?.registrationId,
      currency: 'AED',
      paymentType: 'DB',
      'standingInstruction.type': 'RECURRING',
      ...subType,
      'job.endDate': new Date(APidata?.end_date)
        .toISOString()
        .slice(0, 19)
        .replace('T', ' '),
      'customParameters[payload]': JSON.stringify({
        ...payload,
      }),
    };

    console.log({ payloadData });

    const data = new URLSearchParams(payloadData).toString();
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
