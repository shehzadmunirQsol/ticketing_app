import { router, publicProcedure } from '../trpc';
import { TRPCError } from '@trpc/server';
import { prisma } from '~/server/prisma';
import { signupCustomerSchema,loginCustomerSchema } from '~/schema/customer';

export const customerRouter = router({
  register: publicProcedure
    .input(signupCustomerSchema)
    .mutation(async ({ input }) => {
      try {
        // CustomerData Payload
        const payload = {
          username: input.username,
          email: input.email,
          password: input.password,
          firstname: input.firstname,
          lastname: input.lastname,
        };
        console.log(payload, 'payload');
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

          const customer = await prisma.customer?.create({
            data: payload,
          });

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
        console.log({ input }, "input");
        if (input.email === undefined) {
          throw new TRPCError({
            code: "BAD_REQUEST",
            message: "Enter Email",
          });
        }
        // const user = await prisma.customer?.findFirst({
        //   where: { email: input?.email },
        // });

        // if (!user) {
        //   throw new TRPCError({
        //     code: "NOT_FOUND",
        //     message: "User not found!",
        //   });
        // }
        // return { user };
      } catch (error: any) {
        console.log({ error });
        throw new TRPCError({
          code: "INTERNAL_SERVER_ERROR",
          message: error.message,
        });
      }
    }),
    




});
