import { router, publicProcedure } from '../trpc';
import { TRPCError } from '@trpc/server';
import { contactSchema } from '~/schema/contact';
import { sendEmail } from '~/utils/helper';

export const contactRouter = router({
  contact: publicProcedure
    .input(contactSchema)
    .mutation(async ({ ctx, input }) => {
      try {
        const payload: any = {
          ...input,
          number: input.code + input.number,
        };
        if (payload?.code) delete payload.code;
        const mailOptions = {
          template_id: 4,
          from: payload.email,
          to: process.env.ADMIN as string,
          subject: 'Contact Request ',
          params: {
            user_name: payload.name,
            user_email: payload.email,
            mobile: payload.number,
            message: payload.message,
          },
        };

        await sendEmail(mailOptions);

        return { message: 'Email sent successfully', status: true };
      } catch (error: any) {
        throw new TRPCError({
          code: 'INTERNAL_SERVER_ERROR',
          message: error.message,
        });
      }
    }),
});
