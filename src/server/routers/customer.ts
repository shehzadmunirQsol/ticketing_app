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
  logoutSchema,
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
      include:{
        CustomerAddress:true
      }
    });

    if (!user)
      throw new TRPCError({
        code: 'NOT_FOUND',
        message: 'User not found!',
      });
    const { password, otp, ...userApiData } = user;

    return { data: userApiData };
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

      if (input.is_approved) {
        const mailOptions = {
          template_id: 10,
          from: 'no-reply@winnar.com',
          subject: 'Thank you for sigining up for Winnar',
          to: customer.email,
          params: {
            first_name: customer?.first_name,
          },
        };

        const mailResponse = await sendEmail(mailOptions);
      }
      return customer;
    }),

  getCustomers: publicProcedure
    .input(getCustomerSchema)
    .query(async ({ input }) => {
      try {
        const { filters, ...payload } = input;
        const filterPayload: any = { ...filters };

        if (filterPayload?.searchQuery) delete filterPayload.searchQuery;
        if (filterPayload?.endDate) delete filterPayload.endDate;
        if (filterPayload?.startDate) delete filterPayload.startDate;
        const where: any = { is_deleted: false, ...filterPayload };
        console.log({ filters }, 'filters_input');
        if (input?.filters?.searchQuery) {
          where.OR = [];
          where.OR.push({
            first_name: {
              contains: input?.filters?.searchQuery,
              mode: 'insensitive',
            },
          });
          where.OR.push({
            last_name: {
              contains: input?.filters?.searchQuery,
              mode: 'insensitive',
            },
          });
          where.OR.push({
            username: {
              contains: input?.filters?.searchQuery,
              mode: 'insensitive',
            },
          });
          // options.where.OR.push({
          //   price: { contains: input.searchQuery, mode: 'insensitive' },
          // });
        }

        if (input?.filters?.startDate) {
          const startDate = new Date(input?.filters?.startDate);
          where.created_at = { gte: startDate };
        }
        if (input?.filters?.endDate) {
          const endDate = new Date(input?.filters?.endDate);
          where.created_at = { lte: endDate };
        }

        const totalCategoryPromise = prisma.customer.count({
          where: where,
        });

        const categoryPromise = prisma.customer.findMany({
          orderBy: { created_at: 'desc' },
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
            message: 'Customer not found',
          });
        }

        return {
          message: 'Custoemer found',
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
            from: 'no-reply@winnar.com',
            to: input.email,
            subject: 'Email Verification OTP CODE',
            params: {
              otp: respCode,
              first_name: input?.firstname,
            },
          };
          const mailResponse = await sendEmail(mailOptions);
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

        if (!user || user?.is_deleted) {
          throw new TRPCError({
            code: 'NOT_FOUND',
            message: 'User Not Found',
          });
        }

        if (!user.is_approved) {
          const respCode = await generateOTP(4);
          await prisma.customer?.update({
            where: {
              id: user.id,
            },
            data: { otp: respCode },
          });

          const mailOptions: any = {
            template_id: 2,
            from: 'no-reply@winnar.com',
            to: user.email,
            subject: 'Email Verification OTP CODE',
            params: {
              otp: respCode,
              first_name: user?.first_name,
            },
          };
          const mailResponse = await sendEmail(mailOptions);

          throw new TRPCError({
            code: 'NOT_FOUND',
            message: 'Your Account is Not Verified',
          });
        }

        if (user?.is_disabled) {
          throw new TRPCError({
            code: 'NOT_FOUND',
            message: 'Your Account is Disabled Kindly Contact From Admin',
          });
        }
        const checkPass = await isSamePass(input.password, user?.password);
        if (!checkPass) {
          throw new TRPCError({
            code: 'BAD_REQUEST',
            message: 'Invalid credentials!',
          });
        }
        const jwt = signJWT({ email: user.email, id: user.id });
        const serialized = serialize('winnar-token', jwt, {
          httpOnly: true,
          path: '/',
          sameSite: 'strict',
        });

        ctx?.res?.setHeader('Set-Cookie', serialized);
        const { password, otp, ...userApiData } = user;

        return { user: userApiData, jwt };
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

        if (user?.is_disabled) {
          throw new TRPCError({
            code: 'NOT_FOUND',
            message:
              'Your Account is Disabled Kindly Contact From Admin Thankyou!',
          });
        }

        const respCode = await generateOTP(4);

        //  email
        const mailOptions = {
          template_id: 5,
          from: 'no-reply@winnar.com',
          to: input.email,
          subject: 'Forgot Password request to Winnar',
          params: {
            link: `${process.env.NEXT_PUBLIC_BASE_URL}/reset-password?verification_code=${respCode}&email=${user.email}`,
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
        } else {
          throw new TRPCError({
            code: 'NOT_FOUND',
            message: 'Please try again',
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

        const validity = isValidEmail(input.emailOrUser)
          ? { email: input.emailOrUser }
          : { username: input.emailOrUser };
        const user = await prisma.customer.findFirst({
          where: validity,
        });

        if (!user) {
          throw new TRPCError({
            code: 'NOT_FOUND',
            message: 'Invalid Otp',
          });
        }

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
              is_verified: true,
              is_approved: true,
              otp: '',
            },
          });
        }
        const jwt = signJWT({ email: user.email, id: user.id });
        const serialized = serialize('winnar-token', jwt, {
          httpOnly: true,
          path: '/',
          sameSite: 'strict',
        });

        ctx?.res?.setHeader('Set-Cookie', serialized);
        const { password, otp, ...userApiData } = user;

        return { user: userApiData, jwt };
      } catch (error: any) {
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
        const validity = isValidEmail(input.emailOrUser)
          ? { email: input.emailOrUser }
          : { username: input.emailOrUser };

        const user = await prisma.customer.findFirst({
          where: validity,
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
            from: 'no-reply@winnar.com',
            to: updateResponse.email,
            subject: 'Email Verification OTP CODE',
            params: {
              otp: respCode,
              first_name: updateResponse?.email,
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
            state: '',
            street_address_2: '',
            ...input,
          };
          console.log({ payload }, 'payload bk');
          const customer_address = await prisma.customerAddress?.create({
            data: payload,
          });

          return { customer_address: customer_address, status: true };
        }
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
          postal_code: Number(input.postal_code),
          state: '',
          street_address_2: '',
          ...input,
        };
        console.log({ payload }, 'payload update bk');
        const customer: any = await prisma.customerAddress.update({
          where: {
            id: input.id,
          },
          data: payload,
        });

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
            include: { Customer: {} },
          });

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
        const { password, otp, ...userApiData } = updateResponse;

        return { user: userApiData, status: true };
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
        const updateResponse = await prisma.customer?.update({
          where: {
            id: user.id,
          },
          data: {
            is_disabled: true,
          },
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

  logout: publicProcedure.input(logoutSchema).mutation(async ({ ctx }) => {
    try {
      const serialized = serialize('winnar-token', '', {
        httpOnly: true,
        path: '/',
        sameSite: 'strict',
        // secure: process.env.NODE_ENV !== "development",
      });
      console.log('Serialized :: ', serialized);
      ctx?.res?.setHeader('Set-Cookie', serialized);
      return { message: 'Logout successfully!' };
    } catch (error: any) {
      throw new TRPCError({
        code: 'INTERNAL_SERVER_ERROR',
        message: error?.message,
      });
    }
  }),
});
