import { router, publicProcedure } from '../trpc';
import { TRPCError } from '@trpc/server';
import {
  loginSchema,
  registerSchema,
  logoutSchema,
  updateUserSchema,
  deleteUserSchema,
  getAdminSchema,
} from '~/schema/adminUser';
import { prisma } from '~/server/prisma';
import { hashPass, isSamePass } from '~/utils/hash';
import { serialize } from 'cookie';
import { signJWT, verifyJWT } from '~/utils/jwt';

export const adminUserRouter = router({
  me: publicProcedure.input(getAdminSchema).query(async ({ ctx }) => {
    const token = ctx?.req?.cookies['winnar-admin-token'];
    console.log({ token });

    let userData;
    if (token) {
      userData = await verifyJWT(token);
    } else {
      throw new TRPCError({
        code: 'NOT_FOUND',
        message: 'Token not found!',
      });
    }

    const user = await prisma.adminUser.findUnique({
      where: { id: userData.id },
      select: {
        id: true,
        name: true,
        email: true,
        role_id: true,
      },
    });

    if (!user)
      throw new TRPCError({
        code: 'NOT_FOUND',
        message: 'User not found!',
      });

    return user;
  }),

  login: publicProcedure.input(loginSchema).mutation(async ({ input, ctx }) => {
    try {
      console.log('input.email', input.email);
      const user = await prisma.adminUser.findFirst({
        where: { email: input.email },
      });
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
      const serialized = serialize('winnar-admin-token', jwt, {
        httpOnly: true,
        path: '/',
        sameSite: 'strict',
      });

      ctx?.res?.setHeader('Set-Cookie', serialized);

      return { user, jwt };
    } catch (e: any) {
      console.log('data error', e);

      throw new TRPCError({
        code: 'INTERNAL_SERVER_ERROR',
        message: e.message,
      });
    }
  }),

  register: publicProcedure
    .input(registerSchema)
    .mutation(async ({ input }) => {
      console.log('INPUT :: ', input);
      try {
        const exists: any = await prisma.adminUser.findFirst({
          where: { email: input.email },
        });

        if (exists) {
          throw new TRPCError({
            code: 'FORBIDDEN',
            message: 'User already exists.',
          });
        }
        const hashPassword = await hashPass(input.password);
        console.log('HASH Pass : ', hashPassword);
        const paylaod: any = {
          email: input.email,
          password: hashPassword,
          name: input?.name,
          role_id: input?.role_id,
        };
        console.log(paylaod, 'paylaod');
        const user = await prisma?.adminUser?.create({
          data: paylaod,
        });

        return { user };
      } catch (error: any) {
        throw new TRPCError({
          code: 'INTERNAL_SERVER_ERROR',
          message: error.message,
        });
      }
    }),

  logout: publicProcedure.input(logoutSchema).mutation(async ({ ctx }) => {
    try {
      const serialized = serialize('winnar-admin-token', '', {
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

  updateUser: publicProcedure
    .input(updateUserSchema)
    .mutation(async ({ ctx, input }) => {
      try {
        const paylaod: any = { ...input };
        delete paylaod?.id;

        const user = await prisma.adminUser.update({
          where: { id: input.id },
          data: paylaod,
        });

        if (!user) {
          throw new TRPCError({
            code: 'FORBIDDEN',
            message: 'User not registered!',
          });
        }
        const jwt = signJWT({ email: user.email, id: user.id });

        const serialized = serialize('winnar-admin-token', jwt, {
          httpOnly: true,
          path: '/',
          sameSite: 'strict',
          // secure: process.env.NODE_ENV !== "development",
        });

        ctx.res?.setHeader('Set-Cookie', serialized);

        return { user, jwt };
      } catch (error: any) {
        console.log('data error', error);

        throw new TRPCError({
          code: 'INTERNAL_SERVER_ERROR',
          message: error?.message,
        });
      }
    }),

  deleteUser: publicProcedure
    .input(deleteUserSchema)
    .mutation(async ({ input }) => {
      try {
        const user = await prisma.adminUser.update({
          where: { id: input.id },
          data: {
            is_deleted: true,
          },
        });
        if (!user) {
          throw new TRPCError({
            code: 'FORBIDDEN',
            message: 'User not Found',
          });
        }
        return { user };
      } catch (error: any) {
        console.log('data error', error);

        throw new TRPCError({
          code: 'INTERNAL_SERVER_ERROR',
          message: error?.message,
        });
      }
    }),
});
