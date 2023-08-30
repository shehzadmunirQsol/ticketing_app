import { router, publicProcedure } from '../trpc';
import { TRPCError } from '@trpc/server';
import { prisma } from '~/server/prisma';
import {
  signupCustomerSchema,
  loginCustomerSchema,
  forgotPasswordCustomerSchema,
  resetPasswordCustomerSchema,
  verificationOtpCustomerSchema,
} from '~/schema/customer';
import { hashPass, isSamePass } from '~/utils/hash';
import { signJWT } from '~/utils/jwt';
import { serialize } from 'cookie';
import { generateOTP, isValidEmail, sendEmail } from '~/utils/helper';

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

          const respCode = await generateOTP(4);

          const hashPassword = await hashPass(input.password);
          console.log('HASH Pass : ', hashPassword);

          // CustomerData Payload
          const payload = {
            username: input.username,
            email: input.email,
            password: hashPassword,
            first_name: input.firstname,
            last_name: input.lastname,
            otp: respCode,
          };
          console.log(payload, 'payload');

          const customer = await prisma.customer?.create({
            data: payload,
          });

          const mailOptions: any = {
            template_id: 2,
            from: 'shehzadmunir.qsols@gmail.com',
            to: input.email,
            subject: 'Email Verification OTP CODE',
            params: {
              otp: respCode,
              first_name: input?.firstname,
            },
          };
          const mailResponse = await sendEmail(mailOptions);
          console.log(mailResponse, 'mailResponse');

          console.log(customer, 'user');
          return customer;
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
        const validity = isValidEmail(input.user)
          ? { email: input.user }
          : { username: input.user };

        const user = await prisma.customer.findFirst({
          where: validity,
        });
        console.log('user found: ', user);
        if (!user || user?.is_deleted) {
          throw new TRPCError({
            code: 'NOT_FOUND',
            message: 'User Not Found',
          });
        }
        const checkPass = await isSamePass(input.password, user?.password);
        if (!checkPass) {
          throw new TRPCError({
            code: 'BAD_REQUEST',
            message: 'Password is incorrect',
          });
        }
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
          from: 'shehzadmunir.qsols@gmail.com',
          to: input.email,
          subject: 'Forgot Password request to Winnar',
          link: `:http://localhost:3000/reset-password?verification_code=${respCode}&email=${user.email}`,
        };
        const mailResponse = await sendEmail(mailOptions);
        const updateResponse = await prisma.customer?.update({
          where: {
            id: user.id,
          },
          data: {
            otp: respCode,
          },
        });
        console.log(updateResponse, 'updateResponse');
        return updateResponse;
      } catch (error: any) {
        console.log({ error });
        throw new TRPCError({
          code: 'INTERNAL_SERVER_ERROR',
          message: error.message,
        });
      }
    }),

  resetPasswordCustomer: publicProcedure
    .input(resetPasswordCustomerSchema)
    .mutation(async ({ ctx, input }) => {
      try {
        const user = await prisma.customer.findFirst({
          where: { email: input?.email },
        });
        if (!user || user?.is_deleted) {
          throw new TRPCError({
            code: 'NOT_FOUND',
            message: 'User Not Found',
          });
        }
        if (user.otp === input?.otp) {
          const updateResponse = await prisma.customer?.update({
            where: {
              id: user.id,
            },
            data: {
              otp: '',
            },
          });
          console.log(updateResponse, 'updateResponse');
        } else {
          throw new TRPCError({
            code: 'NOT_FOUND',
            message: 'please try again',
          });
        }

        if (input.password !== input.confirmPassword) {
          throw new TRPCError({
            code: 'NOT_FOUND',
            message: 'Password are not Matching',
          });
        }

        const hashPassword = await hashPass(input.password);
        console.log('HASH Pass : ', hashPassword);
        const updateResponse = await prisma.customer?.update({
          where: {
            id: user.id,
          },
          data: {
            password: hashPassword,
          },
        });
        console.log(updateResponse, 'userCode');

        return updateResponse;
      } catch (error: any) {
        console.log({ error });
        throw new TRPCError({
          code: 'INTERNAL_SERVER_ERROR',
          message: error.message,
        });
      }
    }),

  verificationOtpCustomer: publicProcedure
    .input(verificationOtpCustomerSchema)
    .mutation(async ({ ctx, input }) => {
      try {
        const otpCode = `${input.otp_1}${input.otp_2}${input.otp_3}${input.otp_4}`;

        const user: any = await prisma.customer.findFirst({
          where: { email: input.email, otp: otpCode },
        });

        console.log(user, 'user');
        if (user.otp !== otpCode) {
          throw new TRPCError({
            code: 'NOT_FOUND',
            message: 'Invalid Otp',
          });
        } else {
          const updateResponse = await prisma.customer?.update({
            where: {
              id: user.id,
            },
            data: {
              is_verified:true,
              otp: '',
            },
          });
          console.log(updateResponse, 'updateResponse');
        }
        return { message: 'otp', status: true };
      } catch (error: any) {
        console.log({ error });
        throw new TRPCError({
          code: 'INTERNAL_SERVER_ERROR',
          message: error.message,
        });
      }
    }),
});
