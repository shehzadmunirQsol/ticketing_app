import { router, publicProcedure } from '../trpc';
import { TRPCError } from '@trpc/server';
import { prisma } from '~/server/prisma';
import {
  signupCustomerSchema,
  loginCustomerSchema,
  forgotPasswordCustomerSchema,
  resetPasswordCustomerSchema,
  verificationOtpCustomerSchema,
  getCustomerSchema,
  updateCustomerSchema,
  resendOtpCustomerSchema,
  addCustomerAddress,
  getCustomerAddress,



  accountsDetailSchema,
  passwordChangeSchema,
  deleteMyAccountCustomerSchema,
} from '~/schema/customer';
import { hashPass, isSamePass } from '~/utils/hash';
import { signJWT, verifyJWT } from '~/utils/jwt';
import { serialize } from 'cookie';
import { generateOTP, isValidEmail, sendEmail } from '~/utils/helper';

export const customerRouter = router({
  get: publicProcedure.query(async ({ ctx }) => {
    const token = ctx?.req?.cookies['winnar-token'];
    console.log({ token });

    let userData;
    if (token) {
      userData = await verifyJWT(token);
    } else {
      return { data: null };
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

  update: publicProcedure
    .input(updateCustomerSchema)
    .mutation(async ({ input }) => {
      // const payload = [...input];
      const payload: any = { ...input };
      if (input?.id) delete payload?.id;
      const customer = await prisma.customer.update({
        where: {
          id: input?.id,
        },
        data: { ...payload },
      });
      return customer;
    }),

  getCustomers: publicProcedure
    .input(getCustomerSchema)
    .query(async ({ input }) => {
      try {
        const where: any = { is_deleted: false };

        if (input?.startDate) {
          const startDate = new Date(input?.startDate);
          where.created_at = { gte: startDate };
        }
        if (input?.endDate) {
          const endDate = new Date(input?.endDate);
          where.created_at = { lte: endDate };
        }

        const totalCategoryPromise = prisma.customer.count({
          where: where,
        });

        const categoryPromise = prisma.customer.findMany({
          orderBy: { created_at: 'asc' },
          skip: input.first * input.rows,
          take: input.rows,
          where: where,
        });

        const [totalCustomers, customer] = await Promise.all([
          totalCategoryPromise,
          categoryPromise,
        ]);

        if (!customer?.length) {
          throw new TRPCError({
            code: 'NOT_FOUND',
            message: 'Categories not found',
          });
        }

        return {
          message: 'categories found',
          count: totalCustomers,
          data: customer,
        };
      } catch (error: any) {
        throw new TRPCError({
          code: 'INTERNAL_SERVER_ERROR',
          message: error?.message,
        });
      }
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
          const isEmailExist = await prisma.customer?.findFirst({
            where: {
              email: input.email,
            },
          });
          const isUsernameExist = await prisma.customer?.findFirst({
            where: {
              username: input.username,
            },
          });

          if (isEmailExist) {
            throw new TRPCError({
              code: 'FORBIDDEN',
              message: 'Email Already Exists!',
            });
          }
          if (isUsernameExist) {
            throw new TRPCError({
              code: 'FORBIDDEN',
              message: 'Username Already Exists!',
            });
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
        if (!user.is_approved) {
          throw new TRPCError({
            code: 'NOT_FOUND',
            message: 'Please Wait for Admin Verification',
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
          template_id: 5,
          from: 'shehzadmunir.qsols@gmail.com',
          to: input.email,
          subject: 'Forgot Password request to Winnar',
          params: {
            link: `${process.env.NEXT_PUBLIC_BASE_URL}reset-password?verification_code=${respCode}&email=${user.email}`,
          },
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
        console.log(input, 'SJAHHSJSHJA');
        const otpCode = `${input.otp_1}${input.otp_2}${input.otp_3}${input.otp_4}`;
        console.log(otpCode, input.email, 'HJDJDHDDN');
        const user: any = await prisma.customer.findFirst({
          where: { otp: otpCode },
        });
        console.log(user, 'user HJDJDHDDN');

        if (user.otp !== otpCode) {
          throw new TRPCError({
            code: 'NOT_FOUND',
            message: 'Invalid Otp',
          });
        } else {
          console.log('else HJDJDHDDN');
          const updateResponse = await prisma.customer?.update({
            where: {
              id: user.id,
            },
            data: {
              is_verified: true,
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

  resendOtpCustomer: publicProcedure
    .input(resendOtpCustomerSchema)
    .mutation(async ({ ctx, input }) => {
      try {
        console.log(input, 'SJAHHSJSHJA');
        console.log(input.email, 'HJDJDHDDN');
        const user: any = await prisma.customer.findFirst({
          where: { email: input.email },
        });
        console.log(user, 'user HJDJDHDDN');

        if (!user) {
          throw new TRPCError({
            code: 'NOT_FOUND',
            message: 'User not found',
          });
        } else {
          console.log('else HJDJDHDDN');
          const respCode = await generateOTP(4);
          const updateResponse = await prisma.customer?.update({
            where: {
              id: user.id,
            },
            data: {
              otp: respCode,
            },
          });
          console.log(updateResponse, 'updateResponse');
          const mailOptions: any = {
            template_id: 2,
            from: 'shehzadmunir.qsols@gmail.com',
            to: input.email,
            subject: 'Email Verification OTP CODE',
            params: {
              otp: respCode,
              first_name: input?.email,
            },
          };
          const mailResponse = await sendEmail(mailOptions);
          console.log(mailResponse, 'mailResponse');
        }

        return { user: user, status: true };
      } catch (error: any) {
        console.log({ error });
        throw new TRPCError({
          code: 'INTERNAL_SERVER_ERROR',
          message: error.message,
        });
      }
    }),

  addAddress: publicProcedure
    .input(addCustomerAddress)
    .mutation(async ({ ctx, input }) => {
      try {
        const user: any = await prisma.customer.findFirst({
          where: { id: input.customer_id },
        });
        console.log(user, 'user backend');

        if (!user) {
          throw new TRPCError({
            code: 'NOT_FOUND',
            message: 'User not found',
          });
        } else {
          // here u will do the mutation

          const payload = {

            state: "",
            street_address_2: "",
            ...input,
          }
          console.log({ payload }, "payload bk")
          const customer: any = await prisma.customerAddress.create(
            { data: payload }
          )

        }


        return { user: user, status: true };
      } catch (error: any) {
        console.log({ error });
        throw new TRPCError({
          code: 'INTERNAL_SERVER_ERROR',
          message: error.message,
        });
      }
    }),
  updateAddress: publicProcedure
    .input(addCustomerAddress)
    .mutation(async ({ ctx, input }) => {
      try {

        // here u will do the mutation

        const payload = {

          state: "",
          street_address_2: "",
          ...input,
        }
        console.log({ payload }, "payload update bk")
        const customer: any = await prisma.customerAddress.update({
          where: {
            id: input.customer_id
          },
          data: payload
        })


        return { customer: customer, status: true };

      } catch (error: any) {
        console.log({ error });
        throw new TRPCError({
          code: 'INTERNAL_SERVER_ERROR',
          message: error.message,
        });
      }
    }),
  getAddress: publicProcedure
    .input(getCustomerAddress)
    .query(async ({ ctx, input }) => {
      try {
        const user: any = await prisma.customer.findFirst({
          where: { id: input.customer_id },
        });
        console.log(user, 'user backend');

        if (!user) {
          throw new TRPCError({
            code: 'NOT_FOUND',
            message: 'User not found',
          });

        } else {
          // here u will do the mutation
          const customer: any = await prisma.customerAddress.findFirst({
            where: { customer_id: input.customer_id },
            include: { Customer: {} }
          })

          return customer;
        }

      } catch (error: any) {
        console.log({ error });
        throw new TRPCError({
          code: 'INTERNAL_SERVER_ERROR',
          message: error.message,
        });
      }
    }),

  updateCustomerAccountDetail: publicProcedure
    .input(accountsDetailSchema)
    .mutation(async ({ ctx, input }) => {
      try {
        console.log(input, 'input');
        const user: any = await prisma.customer.findFirst({
          where: { email: input.email },
        });
        const payload = { ...input };
        if (!payload?.dob) delete payload?.dob;

        const updateResponse = await prisma.customer?.update({
          where: {
            id: user.id,
          },
          data: {
            ...payload,
          },
        });

        return { user: user, status: true };
      } catch (error: any) {
        console.log({ error });
        throw new TRPCError({
          code: 'INTERNAL_SERVER_ERROR',
          message: error.message,
        });
      }
    }),

  updateCustomerPassword: publicProcedure
    .input(passwordChangeSchema)
    .mutation(async ({ ctx, input }) => {
      try {
        console.log(input, 'input');
        const user: any = await prisma.customer.findFirst({
          where: { email: input.email },
        });

        const checkPass = await isSamePass(
          input.currentPassword,
          user?.password,
        );
        if (!checkPass) {
          throw new TRPCError({
            code: 'BAD_REQUEST',
            message: 'Old Password is incorrect',
          });
        }

        if (input.newPassword !== input.confirmPassword) {
          throw new TRPCError({
            code: 'NOT_FOUND',
            message: 'Password are not Matching',
          });
        }

        const hashPassword = await hashPass(input.newPassword);
        console.log('HASH Pass : ', hashPassword);
        const updatePasswordResult = await prisma.customer?.update({
          where: {
            id: user.id,
          },
          data: {
            password: hashPassword,
          },
        });
        console.log(updatePasswordResult, 'userCode');
        return { user: user, status: true };
      } catch (error: any) {
        console.log({ error });
        throw new TRPCError({
          code: 'INTERNAL_SERVER_ERROR',
          message: error.message,
        });
      }
    }),
  deleteMyAccountCustomer: publicProcedure
    .input(deleteMyAccountCustomerSchema)
    .mutation(async ({ ctx, input }) => {
      try {
        console.log(input, 'HSJGHSGHSJGSH');
        const user: any = await prisma.customer.findFirst({
          where: { email: input.email },
        });

        const reason = JSON.stringify(input.reasons);
        const payload: any = {
          customer_id: user.id,
          is_deleted: true,
          reason: reason,
          comment: input.message,
        };

        const customer = await prisma.deleteRequest.create({
          data: payload,
        });
        console.log(customer), 'customer';
        return { user: user, status: true };
      } catch (error: any) {
        console.log({ error });
        throw new TRPCError({
          code: 'INTERNAL_SERVER_ERROR',
          message: error.message,
        });
      }
    }),
});
