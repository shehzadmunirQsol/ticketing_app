import { router, publicProcedure } from '../trpc';
import { TRPCError } from '@trpc/server';
import { prisma } from '~/server/prisma';
import {
  signupCustomerSchema,
  loginCustomerSchema,
  forgotPasswordCustomerSchema,
} from '~/schema/customer';
import { hashPass, isSamePass } from '~/utils/hash';
import { signJWT } from '~/utils/jwt';
import { serialize } from 'cookie';
import { generateOTP, sendEmail } from '~/utils/helper';

export const customerRouter = router({
  register: publicProcedure
    .input(signupCustomerSchema)
    .mutation(async ({ input }) => {
      try {
        console.log(input, 'input work them');
        if (!input.email) {
          throw new TRPCError({
            code: 'FORBIDDEN',
            message: 'Customer not registered!',
          });
        } else {
          const isExist = await prisma.customer?.findFirst({
            where: {
              email: input.email,
            },
          });

          if (isExist) {
            return isExist;
          }
          let hashPassword = await hashPass(input.password);
          console.log('HASH Pass : ', hashPassword);

          // CustomerData Payload
          const payload = {
            user_name: input.username,
            email: input.email,
            password: hashPassword,
            first_name: input.firstname,
            last_name: input.lastname,
          };
          console.log(payload, 'payload');

          // const customer = await prisma.customer?.create({
          //   data: payload,
          // });

          // console.log(customer, 'user');
          return payload;
        }
      } catch (error: any) {
        throw new TRPCError({
          code: 'INTERNAL_SERVER_ERROR',
          message: error?.message,
        });
      }
    }),

  loginCustomer: publicProcedure
    .input(loginCustomerSchema)
    .mutation(async ({ ctx, input }) => {
      try {
        const user = await prisma.customer.findFirst({
          where: { email: input.email },
        });
        console.log('user found: ', user);
        if (!user || user?.is_deleted) {
          throw new TRPCError({
            code: 'NOT_FOUND',
            message: 'User Not Found',
          });
        }
        console.log('Inout Pass : ', input.password);
        console.log('User Pass : ', user?.password);
        // let checkPass = await isSamePass(input.password, user?.password);
        // console.log('check pass', checkPass);

        // if (!checkPass) {
        //   throw new TRPCError({
        //     code: 'BAD_REQUEST',
        //     message: 'Password is incorrect',
        //   });
        // }
        const jwt = signJWT({ email: user.email, id: user.id });
        const serialized = serialize('winnar-token', jwt, {
          httpOnly: true,
          path: '/',
          sameSite: 'strict',
        });

        ctx?.res?.setHeader('Set-Cookie', serialized);

        return { user, jwt };
      } catch (error: any) {
        console.log({ error });
        throw new TRPCError({
          code: 'INTERNAL_SERVER_ERROR',
          message: error.message,
        });
      }
    }),

  forgotPasswordCustomer: publicProcedure
    .input(forgotPasswordCustomerSchema)
    .mutation(async ({ ctx, input }) => {
      try {
        console.log(input.email, 'input');
        const user = await prisma.customer.findFirst({
          where: { email: input.email },
        });
        console.log(user, 'mein hun user');
        console.log('user found: ', user);
        if (!user || user?.is_deleted) {
          throw new TRPCError({
            code: 'NOT_FOUND',
            message: 'User Not Found',
          });
        }

        const respCode = await generateOTP(4);
        const mailOptions = {
          template_id: 2,
          from: 'muzammil.devqsols@gmail.com',
          to: input.email,
          subject: 'Sign up request to Build My Token',
     
            link: `Hello, 
          Here is the OTP: ${respCode}.`,
        };
        const mailResponse = await sendEmail(mailOptions);
        console.log('mailResponse', mailResponse);

        return mailResponse;
      } catch (error: any) {
        console.log({ error });
        throw new TRPCError({
          code: 'INTERNAL_SERVER_ERROR',
          message: error.message,
        });
      }
    }),
});
