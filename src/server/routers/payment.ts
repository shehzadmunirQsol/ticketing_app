import { router, publicProcedure } from '../trpc';
import { TRPCError } from '@trpc/server';
import { createPaymentSchema } from '~/schema/payment';
import { prisma } from '~/server/prisma';
import https from 'https';
import querystring from 'querystring';
export const paymentRouter = router({
  createPayment: publicProcedure
    .input(createPaymentSchema)
    .mutation(async ({ input }) => {
      // const payload = [...input];
      const payload = { ...input };

      const apiRes = await CreatePayment(payload)
        .then((response: any) => {
          if (!response?.result?.parameterErrors) {
            return { data: response, success: true };
          }
          throw new Error(response?.result?.parameterErrors[0].message);
        })
        .catch((error) => {
          throw new Error(error.message);
        });
      if (!input?.registrationId) {
      }

      return apiRes;
    }),
});

async function CreatePayment(APidata: createPaymentSchema) {
  try {
    const path = APidata?.registrationId
      ? `/v1/registrations/${APidata?.registrationId}/payments`
      : '/v1/payments';

    const apiDate: any = APidata?.registrationId
      ? {
          entityId: process.env.TOTAN_ENTITY_ID,
          amount: APidata?.price,
          currency: 'AED',
          paymentType: 'DB',
          'standingInstruction.source': 'CIT',
          'standingInstruction.type': 'UNSCHEDULED',
        }
      : {
          entityId: process.env.TOTAN_ENTITY_ID,
          amount: APidata?.price,
          currency: 'AED',
          paymentBrand: 'VISA',
          paymentType: 'DB',
          'card.number':
            APidata?.card && +APidata?.card?.number.replaceAll(' ', ''),
          'card.holder': APidata?.card && APidata?.card?.holder,
          'card.expiryMonth': APidata?.card && APidata?.card?.expiryMonth,
          'card.expiryYear': APidata?.card && APidata?.card?.expiryYear,
          'card.cvv': APidata?.card && +APidata?.card?.cvv,
          'standingInstruction.mode': 'INITIAL',
          'standingInstruction.source': 'CIT',
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
    console.log(error);
    throw new Error(error.message);
  }
}
