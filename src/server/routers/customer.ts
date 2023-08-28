import { router, publicProcedure } from '../trpc';
import { TRPCError } from '@trpc/server';
import { prisma } from '~/server/prisma';
import {
  signupCustomerSchema,
  loginCustomerSchema,
  forgotPasswordCustomerSchema,
  resetPasswordCustomerSchema,
} from '~/schema/customer';
import { hashPass, isSamePass } from '~/utils/hash';
import { signJWT, verifyJWT } from '~/utils/jwt';
import { serialize } from 'cookie';
import { generateOTP, sendEmail } from '~/utils/helper';

export const customerRouter = router({
  get: publicProcedure.query(async ({ ctx }) => {
    const token = ctx?.req?.cookies['winnar-token'];
    console.log({ token });

    let userData;
    if (token) {
      userData = await verifyJWT(token);
    } else {
      return null;
    }

    console.log({ userData }, 'userData');

    const user = await prisma.customer.findUnique({
      where: { id: userData.id },
    });

    if (!user)
      throw new TRPCError({
        code: 'NOT_FOUND',
        message: 'User not found!',
      });

    return { data: user };
  }),

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
          const hashPassword = await hashPass(input.password);
          console.log('HASH Pass : ', hashPassword);

          // CustomerData Payload
          const payload = {
            username: input.username,
            email: input.email,
            password: hashPassword,
            first_name: input.firstname,
            last_name: input.lastname,
          };
          console.log(payload, 'payload');

          const customer = await prisma.customer?.create({
            data: payload,
          });

          console.log(customer, 'user');
          // return customer;
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
        const checkPass = await isSamePass(input.password, user?.password);
        console.log('check pass', checkPass);

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
});
