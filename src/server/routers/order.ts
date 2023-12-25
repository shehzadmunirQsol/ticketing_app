import { router, publicProcedure } from '../trpc';
import { TRPCError } from '@trpc/server';
import {
  createCheckoutPaymentSchema,
  getOrder,
  getByIDSchema,
  getOrderSchema,
  getCheckoutIDSchema,
  getPaymentStatusSchema,
  deleteCardSchema,
} from '~/schema/order';
import https from 'https';
import countryJSON from '~/data/countries.json';

import { prisma } from '~/server/prisma';
import { EMAILS, EMAIL_TEMPLATE_IDS, sendEmail } from '~/utils/helper';

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
          data: {
            is_deleted: true,
            CartItems: {
              updateMany: {
                where: { cart_id: cart.id },
                data: { is_deleted: true },
              },
            },
          },
        });

        const mailOptions = {
          template_id: EMAIL_TEMPLATE_IDS.ORDER_SUCCESS,
          from: EMAILS.contact,
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
  createCheckout: publicProcedure
    .input(getCheckoutIDSchema)
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

        const paymentRes: any = await CreateCheckout({
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
        console.log('paymentRes', paymentRes?.data);
        return {
          message: 'Order created successfully!',
          checkout: paymentRes,
        };
      } catch (error: any) {
        console.log({ error }, 'error message');
        throw new TRPCError({
          code: 'INTERNAL_SERVER_ERROR',
          message: error?.message,
        });
      }
    }),

  getOrders: publicProcedure.input(getOrder).query(async ({ input }) => {
    try {
      const { filters, ...inputData } = input;
      const filterPayload: any = { ...filters };

      if (filterPayload?.searchQuery) delete filterPayload.searchQuery;
      if (filterPayload?.endDate) delete filterPayload.endDate;
      if (filterPayload?.startDate) delete filterPayload.startDate;
      const where: any = {
        customer_id: inputData?.customer_id,
        is_deleted: false,
        ...filterPayload,
      };
      if (input?.status == 'current') {
        const startDate = new Date();
        where.OrderEvent = {
          every: {
            Event: {
              end_date: {
                gte: startDate,
              },
            },
          },
        };
      }
      if (input?.status == 'past') {
        const startDate = new Date();
        where.OrderEvent = {
          every: {
            Event: {
              end_date: {
                lte: startDate,
              },
            },
          },
        };
      }

      if (input?.filters?.startDate && !input?.filters?.endDate) {
        const startDate = new Date(input?.filters?.startDate);
        where.created_at = { gte: startDate };
      }
      if (input?.filters?.endDate && !input?.filters?.startDate) {
        const endDate = new Date(input?.filters?.endDate);
        where.created_at = { lte: endDate };
      }
      if (input?.filters?.endDate && input?.filters?.startDate) {
        const startDate = new Date(input?.filters?.startDate);
        const endDate = new Date(input?.filters?.endDate);
        where.created_at = { gte: startDate, lte: endDate };
      }
      if (input?.filters?.searchQuery) {
        where.OR = [];
        where.OR.push({
          first_name: {
            contains: input?.filters?.searchQuery,
            mode: 'insensitive',
          },
        });
        where.OR.push({
          last_name: {
            contains: input?.filters?.searchQuery,
            mode: 'insensitive',
          },
        });
        where.OR.push({
          email: {
            contains: input?.filters?.searchQuery,
            mode: 'insensitive',
          },
        });
      }

      const totalOrderPromise = prisma.order.count({
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
        totalOrderPromise,
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
  getOrderEvents: publicProcedure.input(getOrder).query(async ({ input }) => {
    try {
      const { filters, ...inputData } = input;
      const filterPayload: any = { ...filters };

      if (filterPayload?.searchQuery) delete filterPayload.searchQuery;
      if (filterPayload?.endDate) delete filterPayload.endDate;
      if (filterPayload?.startDate) delete filterPayload.startDate;
      const where: any = {
        customer_id: inputData?.customer_id,
        is_deleted: false,
        ...filterPayload,
      };
      // if (input?.status == 'current') {
      //   const startDate = new Date();
      //   where.OrderEvent = {
      //     every: {
      //       Event: {
      //         end_date: {
      //           gte: startDate,
      //         },
      //       },
      //     },
      //   };
      // }
      // if (input?.status == 'past') {
      //   const startDate = new Date();
      //   where.OrderEvent = {
      //     every: {
      //       Event: {
      //         end_date: {
      //           lte: startDate,
      //         },
      //       },
      //     },
      //   };
      // }

      if (input?.filters?.startDate && !input?.filters?.endDate) {
        const startDate = new Date(input?.filters?.startDate);
        where.created_at = { gte: startDate };
      }
      if (input?.filters?.endDate && !input?.filters?.startDate) {
        const endDate = new Date(input?.filters?.endDate);
        where.created_at = { lte: endDate };
      }
      if (input?.filters?.endDate && input?.filters?.startDate) {
        const startDate = new Date(input?.filters?.startDate);
        const endDate = new Date(input?.filters?.endDate);
        where.created_at = { gte: startDate, lte: endDate };
      }
      if (input?.filters?.searchQuery) {
        where.OR = [];
        where.OR.push({
          first_name: {
            contains: input?.filters?.searchQuery,
            mode: 'insensitive',
          },
        });
        where.OR.push({
          last_name: {
            contains: input?.filters?.searchQuery,
            mode: 'insensitive',
          },
        });
        where.OR.push({
          email: {
            contains: input?.filters?.searchQuery,
            mode: 'insensitive',
          },
        });
      }

      const totalEventPromise = prisma.orderEvent.count({
        where: where,
      });

      const eventPromise = prisma.orderEvent.findMany({
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
          Order: {
            select: {
              discount_amount: true,
              sub_total_amount: true,
            },
          },
          Event: {
            select: {
              id: true,
              thumb: true,
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
  get: publicProcedure.input(getOrderSchema).query(async ({ input }) => {
    try {
      const { filters, ...inputData } = input;
      const filterPayload: any = { ...filters };
      console.log(filterPayload, 'filterPayload');

      if (filterPayload?.searchQuery) delete filterPayload.searchQuery;
      if (filterPayload?.endDate) delete filterPayload.endDate;
      if (filterPayload?.startDate) delete filterPayload.startDate;
      const where: any = { is_deleted: false, ...filterPayload };

      if (input?.filters?.startDate && !input?.filters?.endDate) {
        const startDate = new Date(input?.filters?.startDate)
          ?.toISOString()
          .split('T')[0] as string;
        where.created_at = { gte: new Date(startDate) };
      }
      if (input?.filters?.endDate && !input?.filters?.startDate) {
        const inputEndDate = new Date(input?.filters?.endDate);
        const endDate = new Date(inputEndDate.setHours(23, 59));
        where.created_at = { lte: endDate };
      }
      if (input?.filters?.endDate && input?.filters?.startDate) {
        const startDate = new Date(input?.filters?.startDate)
          ?.toISOString()
          .split('T')[0] as string;
        const inputEndDate = new Date(input?.filters?.endDate);
        const endDate = new Date(inputEndDate.setHours(23, 59));
        where.created_at = { gte: new Date(startDate), lte: endDate };
      }

      if (input?.filters?.searchQuery) {
        where.OR = [];
        where.OR.push({
          first_name: {
            contains: input?.filters?.searchQuery,
            mode: 'insensitive',
          },
        });
        where.OR.push({
          last_name: {
            contains: input?.filters?.searchQuery,
            mode: 'insensitive',
          },
        });
        where.OR.push({
          email: {
            contains: input?.filters?.searchQuery,
            mode: 'insensitive',
          },
        });

        where.OR.push({
          Customer: {
            email: {
              contains: input?.filters?.searchQuery,
              mode: 'insensitive',
            },
          },
        });
      }

      const totalEventPromise = prisma.order.count({
        where: where,
      });

      const eventPromise = prisma.order.findMany({
        orderBy: { created_at: 'desc' },
        skip: input.first * input.rows,
        take: inputData.rows,
        where: where,
        include: {
          Customer: {
            select: {
              id: true,
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
      const where: any = { is_deleted: false };
      if (input?.order_id) where.id = input?.order_id;

      if (input?.customer_id) where.customer_id = input.customer_id;

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
                  EventDescription: {
                    where: {
                      lang_id: input?.lang_id,
                    },
                  },
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
  getStatus: publicProcedure
    .input(getPaymentStatusSchema)
    .mutation(async ({ input }) => {
      try {
        // Total Processing Initial Payment Process
        const paymentPayload: any = {
          id: input?.checkout_id,
        };

        const paymentRes: any = await getPaymentStatus({
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
        if (paymentRes?.data) {
          const statusData = paymentRes?.data;
          const successStatus =
            statusData?.result?.description.toLowerCase().includes('success') &&
            statusData?.resultDetails?.resultMessage
              .toLowerCase()
              .includes('success');
          if (successStatus) {
            const payload = JSON.parse(statusData?.customParameters?.payload);
            const customerData = await prisma.customer.findFirst({
              where: {
                id: payload?.values?.customer_id,
              },
            });
            let updateCustomer: any;
            if (
              statusData?.registrationId &&
              (customerData?.total_customer_id === '' ||
                !customerData?.total_customer_id?.includes(
                  statusData?.registrationId,
                ))
            ) {
              const register_id =
                customerData?.total_customer_id !== ''
                  ? customerData?.total_customer_id +
                    ',' +
                    statusData?.registrationId
                  : statusData?.registrationId;
              updateCustomer = await prisma.customer.update({
                where: {
                  id: payload?.values?.customer_id,
                },
                data: {
                  total_customer_id: register_id,
                },
              });
            }

            const cart = await prisma.cart.findUnique({
              where: { id: payload?.values?.cart_id },
              include: {
                CouponApply: {
                  where: { is_deleted: false, is_used: false },
                  select: {
                    id: true,
                    discount: true,
                    is_percentage: true,
                  },
                },
                CartItems: {
                  include: {
                    Event: {
                      include: {
                        EventDescription: {
                          where: {
                            lang_id: 1,
                          },
                        },
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
            const totalPaymentId = paymentRes?.data?.id;
            const { total_id, ...valuesData }: any = { ...payload?.values };

            const orderPayload: any = {
              ...valuesData,
              phone_number:
                payload?.values?.code + payload?.values?.phone_number,

              sub_total_amount: subTotalAmount,
              status: 'paid',
              discount_amount: discountAmount,
              total_amount: subTotalAmount - discountAmount,
              total_payment_id: totalPaymentId,
            };

            if (payload?.values?.code) delete orderPayload?.code;
            if (payload?.values?.cart_id) delete orderPayload?.cart_id;
            if (payload?.values?.total_id) delete orderPayload?.total_id;

            const orderEventPayload = cart?.CartItems.map((item) => ({
              event_id: item.Event.id,
              customer_id: payload?.values?.customer_id,
              ticket_price: item.Event.price,
              quantity: item.quantity,
              is_subscribe: item.is_subscribe,
            }));

            const order = await prisma.order.create({
              select: {
                id: true,
                OrderEvent: {
                  select: {
                    id: true,
                    event_id: true,
                  },
                },
              },
              data: {
                ...orderPayload,
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
                  tickets_sold:
                    (item?.Event?.tickets_sold ?? 0) + item?.quantity,
                },
              }),
            );

            await Promise.all(eventPromises);

            // assigning tickets to customer
            // order.OrderEvent.forEach((orderEvent) => {
            //   (async () => {
            //     const assignedEventTicketCounts =
            //       await prisma.eventTickets.count({
            //         where: {
            //           event_id: orderEvent.event_id,
            //           customer_id: { not: null },
            //         },
            //       });

            //     const cartItem = cart.CartItems.find(
            //       (cartItem) => cartItem.event_id === orderEvent.event_id,
            //     );

            //     await prisma.eventTickets.updateMany({
            //       where: {
            //         event_id: orderEvent.event_id,
            //         customer_id: null,
            //         ticket_num: {
            //           gt: assignedEventTicketCounts,
            //           lte:
            //             assignedEventTicketCounts + (cartItem?.quantity ?? 0),
            //         },
            //       },
            //       data: {
            //         order_event_id: orderEvent.id,
            //         customer_id: payload?.values?.customer_id,
            //       },
            //     });
            //   })();
            // });


            //NEW SHUFFLED DATA---
            const shuffleArray = (array:any) => {
              for (let i = array.length - 1; i > 0; i--) {
                const j = Math.floor(Math.random() * (i + 1));
                [array[i], array[j]] = [array[j], array[i]];
              }
              return array;
            };

            order.OrderEvent.forEach((orderEvent) => {
              (async () => {

                const cartItem = cart.CartItems.find(
                  (cartItem) => cartItem.event_id === orderEvent.event_id,
                );

                const eventTicketsToUpdate = await prisma.eventTickets.findMany({
                  where: {
                    event_id: orderEvent.event_id,
                    customer_id: null,
                  },
                });

                const randomizedEventTickets = shuffleArray(eventTicketsToUpdate);

                const ticketsToUpdate = randomizedEventTickets.slice(0, cartItem?.quantity ?? 0);


                await prisma.eventTickets.updateMany({
                  where: {
                    id: {
                      in: ticketsToUpdate.map((ticket:any) => ticket.id),
                    },
                  },
                  data: {
                    order_event_id: orderEvent.id,
                    customer_id: payload?.values?.customer_id,
                  },
                });

              })();
            });
            //NEW SHUFFLED DATA---







            // const emailPayload = cart?.CartItems.map((item) => ({
            //   name: item?.Event?.EventDescription[0]?.name as string,
            //   price: item.Event.price,
            //   qty: item.quantity,
            //   total_price: item.Event.price * item.quantity,
            // }));

            // const mailOptions = {
            //   template_id: EMAIL_TEMPLATE_IDS.ORDER_SUCCESS,
            //   from: EMAILS.contact,
            //   to: payload.values.email,
            //   subject: 'Your order has been placed 🎉',
            //   params: {
            //     first_name: payload.values.first_name,
            //     status: 'paid',
            //     order_number: order?.id,
            //     total_price:
            //       'AED ' + (subTotalAmount - discountAmount).toLocaleString(),
            //     event_details: emailPayload,
            //     discount: 'AED ' + discountAmount.toLocaleString(),
            //     sub_total: 'AED ' + subTotalAmount.toLocaleString(),
            //   },
            // };

            // await sendEmail(mailOptions);

            await prisma.cart.update({
              where: { id: cart.id },

              data: {
                is_deleted: true,
                CartItems: {
                  updateMany: {
                    where: { cart_id: cart.id },
                    data: { is_deleted: true },
                  },
                },
                CouponApply: {
                  updateMany: {
                    where: {
                      cart_id: cart.id,
                      is_deleted: false,
                      is_used: false,
                    },
                    data: {
                      is_used: true,
                    },
                  },
                },
              },
            });
            // const { password, otp, ...userApiData } = updateCustomer;
            const useAPIData = { ...updateCustomer };
            if (useAPIData?.password) delete useAPIData?.password;
            if (useAPIData?.otp) delete useAPIData?.password;

            return {
              message: paymentRes?.data,
              status: true,
              user: useAPIData,
              order_id: order.id,
            };
          }
        }
        throw new TRPCError({
          code: 'BAD_REQUEST',
          message: 'Something Went Wrong',
        });
      } catch (error: any) {
        console.log({ error }, 'error message');
        throw new TRPCError({
          code: 'INTERNAL_SERVER_ERROR',
          message: error?.message,
        });
      }
    }),
  deleteCard: publicProcedure
    .input(deleteCardSchema)
    .mutation(async ({ input }) => {
      try {
        // Total Processing Initial Payment Process
        const paymentPayload: any = {
          id: input?.registration_id,
        };

        const paymentRes: any = await deleteCard({
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
        console.log({ paymentRes });
        if (paymentRes?.data) {
          const regID = input?.total_customer_id.split(',');
          const filterArray = regID.filter(
            (word, index) => index !== input?.index,
          );
          const updateCustomer = await prisma.customer.update({
            where: {
              id: input?.customer_id,
            },
            data: {
              total_customer_id: filterArray?.join(),
            },
          });
          return {
            message: paymentRes?.data,
            status: true,
          };
        }
        // throw new TRPCError({
        //   code: 'BAD_REQUEST',
        //   message: 'Something Went Wrong',
        // });
      } catch (error: any) {
        console.log({ error }, 'error message');
        throw new TRPCError({
          code: 'INTERNAL_SERVER_ERROR',
          message: error?.message,
        });
      }
    }),
});

async function CreateCheckout(APidata: any) {
  try {
    const path = '/v1/checkouts';
    const payload = { ...APidata };
    console.log(APidata, 'APidata?.paymentBrandsss');
    const registrationID: any = APidata?.values?.total_id?.split(',');
    const regPayload: { [key: string]: string } = {};

    if (
      APidata?.values?.total_id !== '' &&
      registrationID &&
      registrationID.length
    ) {
      registrationID.forEach((item: string, index: number) => {
        regPayload[`registrations[${index}].id`] = item;
      });
    }
    const countries: any = countryJSON.find(
      (item) => item.country == payload?.values?.country,
    );
    console.log(countries['alpha-2'], 'thisiscountryiso');

    if (payload?.card) delete payload?.card;
    if (payload?.values?.total_id) delete payload?.values?.total_id;
    const tot_amount = APidata?.total_amount.toFixed(2);
    const apiDate: any = {
      entityId: process.env.TOTAN_ENTITY_ID,
      amount: APidata?.total_amount.toFixed(2),
      currency: 'AED',
      paymentType: 'DB',
      ...regPayload,
      'billing.country': countries['alpha-2'],
      'billing.street1': payload?.values?.street_address,
      'billing.state': payload?.values?.state,
      'billing.postcode': payload?.values?.postal_code,
      'billing.city': payload?.values?.city,
      'customer.givenName': payload?.values?.first_name,
      'customer.surname': payload?.values?.last_name,
      'customer.email': payload?.values?.email,
      'customer.phone': payload?.values?.phone_number,
      'standingInstruction.source': 'CIT',
      'standingInstruction.mode': 'INITIAL',
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
async function getPaymentStatus(APidata: any) {
  try {
    const path = `/v1/checkouts/${APidata?.id}/payment?entityId=${process.env.TOTAN_ENTITY_ID}`;
    // path += '?entityId=${process.env.TOTAN_ENTITY_ID}';

    const options = {
      port: 443,
      host: 'eu-test.oppwa.com',
      path: path,
      method: 'GET',
      headers: {
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
      postRequest.end();
    });
  } catch (error: any) {
    console.log({ error }, 'function error');
    throw new Error(error.message);
  }
}
async function deleteCard(APidata: any) {
  try {
    const path = `/v1/registrations/${APidata?.id}?entityId=${process.env.TOTAN_ENTITY_ID}`;
    // path += '?entityId=${process.env.TOTAN_ENTITY_ID}';

    const options = {
      port: 443,
      host: 'eu-test.oppwa.com',
      path: path,
      method: 'DELETE',
      headers: {
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
      postRequest.end();
    });
  } catch (error: any) {
    console.log({ error }, 'function error');
    throw new Error(error.message);
  }
}
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
      postRequest.on('error', reject);
      postRequest.write(data);
      postRequest.end();
    });
  } catch (error: any) {
    console.log({ error }, 'function error');
    throw new Error(error.message);
  }
}
