import { router, publicProcedure } from '../trpc';
import { TRPCError } from '@trpc/server';
import { loginSchema, otpSchema } from '~/schema/user';
import { prisma } from '~/server/prisma';
import { generateOTP } from '~/utils/helper';

export const userRouter = router({
  login: publicProcedure.input(loginSchema).mutation(async ({ input, ctx }) => {
    const { email } = input;
    const otp = generateOTP(4);
  }),
});
