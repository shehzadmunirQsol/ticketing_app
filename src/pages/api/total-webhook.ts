import { NextApiRequest, NextApiResponse } from 'next';
import { totalProcessingPayment } from '~/server/clientControllers/totalProcessing';

export default async function handler(
  req: NextApiRequest,
  res: NextApiResponse,
) {
  console.log(req?.body);
  const { method } = req;


  try {
    if (method == 'POST') {
      
      await totalProcessingPayment(req, res);
    }
    // event = stripe.webhooks.constructEvent(rawBody, sig, endpointSecret);
  } catch (err: any) {
    res.status(400).send(`Webhook Error: ${err.message}`);
    return;
  }

  // Return a 200 response to acknowledge receipt of the event
  res.send({ message: 'Received event' });
}
