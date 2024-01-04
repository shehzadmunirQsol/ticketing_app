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
  deleteMyAccountCustomerSchema,
  logoutSchema,
  updateCustomerAddress,
  passwordChangeSchemaInput,
  getCustomerDetailSchema,
} from '~/schema/customer';
import { hashPass, isSamePass } from '~/utils/hash';
import { signJWT, verifyJWT } from '~/utils/jwt';
import { setCookie, deleteCookie } from 'cookies-next';
import { EMAILS, generateOTP, isValidEmail, sendEmail, sendSMS } from '~/utils/helper';
import {
  AddContactPayloadType,
  addContactsToBrevoList,
} from '~/service/api/addContacts.service';

export const customerRouter = router({
  get: publicProcedure.query(async ({ ctx }) => {
    // const token = ctx?.req?.cookies['winnar-token'];
    const token = (ctx?.req?.headers.Authorization as string)?.split(' ')[1];
    console.log({ token });

    let userData;
    if (token) {
      userData = await verifyJWT(token);
    } else {
      return { data: null };
    }

    const user = await prisma.customer.findUnique({
      where: { id: userData.id },
      include: {
        CustomerAddress: true,
      },
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
      const { id, type, ...payload } = input;

      console.log({ payload }, 'payload payload');
      const customer = await prisma.customer.update({
        where: { id },
        data: payload,
      });

      if (type === 'block' && customer.is_blocked) {
        const mailOptions = {
          template_id: 11,
          from: 'no-reply@winnar.com',
          subject: 'Your Account is now Blocked',
          to: customer.email,
          params: {
            first_name: customer?.first_name,
          },
        };
        await sendEmail(mailOptions);
      }

      if (type === 'block' && !customer.is_blocked) {
        const mailOptions = {
          template_id: 44,
          from: 'no-reply@winnar.com',
          subject: 'Your Account is now Unlocked',
          to: customer.email,
          params: {
            first_name: customer?.first_name,
          },
        };
        await sendEmail(mailOptions);
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
          where.OR.push({
            email: {
              contains: input?.filters?.searchQuery,
              mode: 'insensitive',
            },
          });
          // options.where.OR.push({
          //   price: { contains: input.searchQuery, mode: 'insensitive' },
          // });
        }

        if (input?.filters?.startDate && !input?.filters?.endDate) {
          const startDate = new Date(input?.filters?.startDate)
            ?.toISOString()
            .split('T')[0] as string;
          where.created_at = { gte: new Date(startDate) };
        }
        if (input?.filters?.endDate && !input?.filters?.startDate) {
          const endDate = new Date(input?.filters?.endDate)
            ?.toISOString()
            .split('T')[0] as string;
          where.created_at = { lte: new Date(endDate) };
        }
        if (input?.filters?.endDate && input?.filters?.startDate) {
          const startDate = new Date(input?.filters?.startDate)
            ?.toISOString()
            .split('T')[0] as string;
          const endDate = new Date(input?.filters?.endDate)
            ?.toISOString()
            .split('T')[0] as string;

          where.created_at = {
            gte: new Date(startDate),
            lte: new Date(endDate),
          };
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
  getCustomerDetail: publicProcedure
    .input(getCustomerDetailSchema)
    .query(async ({ input }) => {
      try {
        const customerPromise = prisma.customer.findFirst({
          where: { id: input?.customer_id },
          include: {
            CustomerAddress: {
              where: {
                is_default: true,
              },
              select: {
                street_address_1: true,
                street_address_2: true,
                city: true,
                country: true,
                address_type: true,
                postal_code: true,
                phone_code: true,
                phone_number: true,
                state: true,
              },
            },
          },
        });

        const [customer] = await Promise.all([customerPromise]);

        if (!customer) {
          throw new TRPCError({
            code: 'NOT_FOUND',
            message: 'Customer not found',
          });
        }
        const { password, otp, ...userApiData } = customer;

        return {
          message: 'Custoemer found',
          data: userApiData,
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
              is_deleted: false,
            },
          });

          if (isEmailExist) {
            throw new TRPCError({
              code: 'FORBIDDEN',
              message: 'Email Already Exists!',
            });
          }

          if (input.password !== input.confirmpassword) {
            throw new TRPCError({
              code: 'NOT_FOUND',
              message: 'Password are not Matching',
            });
          }

          const respCode = await generateOTP(4);

          const hashPassword = await hashPass(input.password);
          console.log('HASH Pass : ', hashPassword);

          // CustomerData Payload
          const payload: any = {
            ...input,
            phone_number: input?.code + input?.phone_number,

            password: hashPassword,
            otp: respCode,
          };
          if (input?.code) delete payload?.code;

          const { confirmpassword, ...modifiedPayload } = payload;
          const customer = await prisma.customer?.create({
            data: {
              ...modifiedPayload,
              CustomerAddress: {
                createMany: {
                  data: [
                    {
                      country: input?.country,
                      phone_code: input?.code,
                      phone_number: input?.phone_number,
                      is_default: true,
                    },
                  ],
                },
              },
            },
          });

          const mailOptions: any = {
            template_id: 2,
            from: EMAILS.contact,
            to: input.email,
            subject: 'Email Verification OTP CODE',
            params: {
              otp: respCode,
              first_name: input?.first_name,
            },
          };

          await sendEmail(mailOptions);


//SMS***********
          // const smsOptions: any = {
          //   // to: input.phone_number,
          //   to: "971544205311",
          //   subject: 'Enter this code:' +respCode+ ' to validate your account',
          // };
          // await sendSMS(smsOptions);
//SMS***********



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
            from: EMAILS.contact,
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

        if (user?.is_blocked) {
          throw new TRPCError({
            code: 'UNAUTHORIZED',
            message: 'Your Account is Blocked',
          });
        }
        const checkPass = await isSamePass(input.password, user?.password);
        if (!checkPass) {
          throw new TRPCError({
            code: 'INTERNAL_SERVER_ERROR',
            message: 'Invalid credentials!',
          });
        }
        const jwt = signJWT({ email: user.email, id: user.id });

        const { req, res } = ctx;
        setCookie('winnar-token', jwt, {
          req,
          res,
          // httpOnly: true,
          // path: '/',
          // sameSite: 'strict',
        });

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
              'Your Account is Disabled Kindly Contact From Admin Thank you!',
          });
        }

        const respCode = await generateOTP(4);

        //  email
        const mailOptions = {
          template_id: 5,
          from: EMAILS.contact,
          to: input.email,
          subject: 'Forgot Password request to Winnar',

          params: {
            link: `${
              process.env.NEXT_PUBLIC_BASE_URL
            }/reset-password?verification_code=${encodeURIComponent(
              respCode,
            )}&email=${encodeURIComponent(user.email)}`,
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

        const validity = { email: input.emailOrUser, is_deleted: false };
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

          // const resgistranMailOptions = {
          //   template_id: 10,
          //   from: EMAILS.contact,
          //   subject: 'Thank you for signing up in Winnar',
          //   to: user.email,
          //   params: {
          //     first_name: user?.first_name,
          //   },
          // };
          // await sendEmail(resgistranMailOptions);

          const customerAddress = await prisma.customerAddress.findFirst({
            where: { customer_id: updateResponse.id },
          });

          const addContactPayload: AddContactPayloadType = {
            email: updateResponse.email,
            attributes: {},
          };

          if (updateResponse.first_name)
            addContactPayload.attributes.FIRSTNAME = updateResponse.first_name;
          if (updateResponse.last_name)
            addContactPayload.attributes.LASTNAME = updateResponse.last_name;
          if (updateResponse.first_name && updateResponse.last_name)
            addContactPayload.attributes.FULL_NAME = `${updateResponse.first_name} ${updateResponse.last_name}`;
          if (updateResponse.gender)
            addContactPayload.attributes.GENDER =
              updateResponse.gender === 'male' ? '1' : '2';
          if (updateResponse.dob)
            addContactPayload.attributes.DATE_OF_BIRTH =
              updateResponse?.dob?.toISOString()?.split('T')[0] ?? '';
          if (customerAddress?.street_address_1)
            addContactPayload.attributes.ADDRESS =
              customerAddress?.street_address_1;
          if (customerAddress?.city)
            addContactPayload.attributes.CITY = customerAddress?.city;
          if (customerAddress?.country)
            addContactPayload.attributes.COUNTRY = customerAddress?.country;
          if (customerAddress?.state)
            addContactPayload.attributes.STATE = customerAddress?.state;
          if (customerAddress?.phone_code && customerAddress?.phone_number)
            addContactPayload.attributes.PHONE =
              customerAddress?.phone_code + customerAddress?.phone_number;

          await addContactsToBrevoList(addContactPayload);
        }
        const jwt = signJWT({ email: user.email, id: user.id });

        const { req, res } = ctx;

        setCookie('winnar-token', jwt, {
          req,
          res,
          // httpOnly: true,
          // path: '/',
          // sameSite: 'strict',
        });

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

        if (!user) {
          throw new TRPCError({
            code: 'NOT_FOUND',
            message: 'User not found',
          });
        } else {
          const respCode = await generateOTP(4);
          const updateResponse = await prisma.customer?.update({
            where: {
              id: user.id,
            },
            data: {
              otp: respCode,
            },
          });

          const mailOptions: any = {
            template_id: 2,
            from: EMAILS.contact,
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
  updateDefaultAddress: publicProcedure
    .input(updateCustomerAddress)
    .mutation(async ({ ctx, input }) => {
      try {
        // here u will do the mutation

        const payload = {
          ...input,
        };
        await prisma.customerAddress.updateMany({
          where: {
            customer_id: input.customer_id,
          },
          data: {
            is_default: false,
          },
        });
        const customer: any = await prisma.customerAddress.update({
          where: {
            id: input.id,
          },
          data: {
            is_default: true,
          },
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
    .query(async ({ input }) => {
      try {
        // here u will do the mutation
        const addresses = await prisma.customerAddress.findMany({
          where: { customer_id: input.customer_id },
          orderBy: { created_at: 'asc' },
        });

        return addresses;
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
        const { id, ...payload } = input;

        // if (!payload?.dob) delete payload?.dob;
        console.log({ input }, 'acc- detail');

        const updateResponse = await prisma.customer?.update({
          where: {
            id: id,
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
    .input(passwordChangeSchemaInput)
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
      const { req, res } = ctx;
      deleteCookie('winnar-token', {
        req,
        res,
        // httpOnly: true,
        // path: '/',
        // sameSite: 'strict',
      });

      return { message: 'Logout successfully!' };
    } catch (error: any) {
      throw new TRPCError({
        code: 'INTERNAL_SERVER_ERROR',
        message: error?.message,
      });
    }
  }),
});
