import { prisma } from '../prisma';

export async function totalProcessingPayment(req: any, res: any) {
  const input = { ...req.body };
  // delete input.routes;

  try {
    const payload = {
      order_id: 2,
      event_id: 1,
      customer_id: 2,
      ticket_price: 2,
      quantity: 56,
      subscription_type: 'weekly',
    };

    const previousOrder = await prisma.order.findUnique({
      where: { id: payload.order_id },
    });
    if (previousOrder) {
      const orderPayload: any = {
        ...previousOrder,
        sub_total_amount: payload.quantity * payload.ticket_price,
        discount_amount: 0,
        total_amount: payload.quantity * payload.ticket_price,
        total_payment_id: '',
        parent_order_id: payload.order_id,
      };

      if (orderPayload.id) delete orderPayload.id;

      const orderEventPayload = [
        {
          event_id: payload.event_id,
          customer_id: payload?.customer_id,
          ticket_price: payload.ticket_price,
          quantity: payload.quantity,
          is_subscribe: true,
        },
      ];

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

      await prisma.event.update({
        where: { id: payload.event_id },
        data: {
          tickets_sold: {
            increment: payload?.quantity,
          },
        },
      });
      const endingDate = new Date();
      const date = payload?.subscription_type === 'weekly' ? 7 : 30;
      endingDate.setDate(endingDate.getDate() + date);

      const updateSubs = await prisma.orderSubscription.updateMany({
        where: { order_id: payload.order_id, event_id: payload.event_id },
        data: { next_date: endingDate },
      });
      console.log(updateSubs, 'updateSubs');

      // finally send email notification to the customer
    }

    // const orderPayload

    return res.status(200).send({ data: 'data', success: true });
  } catch (err: any) {
    res.status(500).send({ message: err.message as string, success: false });
  }
}
