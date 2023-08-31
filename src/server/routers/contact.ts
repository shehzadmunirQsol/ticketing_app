import { router, publicProcedure } from '../trpc';
import { TRPCError } from '@trpc/server';
import { prisma } from '~/server/prisma';
import { contactUsSchema } from '~/schema/contact';
import { hashPass, isSamePass } from '~/utils/hash';
import { signJWT } from '~/utils/jwt';
import { serialize } from 'cookie';
import { generateOTP, isValidEmail, sendEmail } from '~/utils/helper';

export const contactRouter = router({
  contact: publicProcedure
    .input(contactUsSchema)
    .mutation(async ({ ctx, input }) => {
      try {
        console.log(input, 'input');

        const number = input.code + input.number;
        console.log(number, 'NJSDHSJKDHDNKJ');
        const payload: any = {
          ...input,
          number: input.code + input.number,
        };
        if (payload?.code) delete payload.code;
        const mailOptions = {
          template_id: 4,
          from: 'shehzadmunir.qsols@gmail.com',
          to: 'muzammil.devqsols@gmail.com',
          subject: 'Contact us request to Winnar ',
          params :{
            user_name:payload.name,
            user_email:payload.email,
            mobile:payload.number,
            message:payload.message,
          }
        };

        const mailResponse = await sendEmail(mailOptions);
        console.log(mailResponse);

        return { message: 'Email sent successfully', status: true };
      } catch (error: any) {
        console.log({ error });
        throw new TRPCError({
          code: 'INTERNAL_SERVER_ERROR',
          message: error.message,
        });
      }
    }),
});
