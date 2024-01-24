import { router, publicProcedure } from '../trpc';
import { TRPCError } from '@trpc/server';
import { createPaymentSchema } from '~/schema/payment';
import { prisma } from '~/server/prisma';
import paymentConf from '~/paymentconf/payment.json';
import https from 'https';


var paymentmode = "prod";
var paymenturl = "";
var TOTANENTITYID = "";
var TOTALPROCESSINGBEARERID = "";
if(paymentmode==="prod"){
  paymenturl = paymentConf.PAYMENTURL.prodURL;
  TOTANENTITYID = paymentConf.TOTANENTITY.prodID;
  TOTALPROCESSINGBEARERID = paymentConf.TOTALPROCESSINGBEARER.prodToken;
}else{
  paymenturl = paymentConf.PAYMENTURL.testURL;
  TOTANENTITYID = paymentConf.TOTANENTITY.testID;
  TOTALPROCESSINGBEARERID = paymentConf.TOTALPROCESSINGBEARER.testToken;
}
 
// var TOTANENTITYID = process.env.TOTAN_ENTITY_ID; 
// var TOTALPROCESSINGBEARERID = process.env.TOTAL_PROCESSING_BEARER; 

export const paymentRouter = router({
  createPayment: publicProcedure
    .input(createPaymentSchema)
    .mutation(async ({ input }) => {
      // const payload = [...input];
      const payload = { ...input };

      const apiRes: any = await CreatePayment(payload)
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

      return { apiRes, user, success: true };
    }),
});

async function CreatePayment(APidata: createPaymentSchema) {
  try {
    const path = APidata?.registrationId
      ? `/v1/registrations/${APidata?.registrationId}/payments`
      : '/v1/payments';

    const apiDate: any = APidata?.registrationId
      ? {
          entityId: TOTANENTITYID,
          amount: APidata?.price,
          currency: 'AED',
          paymentType: 'DB',
          'standingInstruction.source': 'CIT',
          // wpwlOptions: JSON.stringify(APidata?.cart),
          'customParameters[payload]': JSON.stringify({
            registrationId: '8ac7a49f8a4ae6c0018a4b56f6a9161b',
            price: 90,
            customer_id: 13,
          }),

          'standingInstruction.type': 'UNSCHEDULED',
        }
      : {
          entityId: TOTANENTITYID,
          amount: APidata?.price,
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
            registrationId: '8ac7a49f8a4ae6c0018a4b56f6a9161b',
            price: 90,
            customer_id: 13,
          }),

          createRegistration: 'true',
        };

    const data = new URLSearchParams(apiDate).toString();
    const options = {
      port: 443,
      host: paymenturl,
      path: path,
      method: 'POST',
      headers: {
        'Content-Type': 'application/x-www-form-urlencoded',
        'Content-Length': data.length,
        Authorization: TOTALPROCESSINGBEARERID,
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
