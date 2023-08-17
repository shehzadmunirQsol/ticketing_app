import { router, publicProcedure } from '../trpc';
import { TRPCError } from '@trpc/server';
import { loginSchema, registerSchema } from '~/schema/user';
import { prisma } from '~/server/prisma';
import {hashPass, isSamePass} from '~/utils/hash';

export const userRouter = router({
    login: publicProcedure
    .input(loginSchema)
    .mutation(async ({ input }) => {
        try {
            const user = await prisma.user.findFirst({
                where: { email: input.email},
            });
            console.log('user found: ', user);
            if (!user) {
            throw new TRPCError({
                code: 'NOT_FOUND',
                message: 'User not found'
            });
            }
            console.log("Inout Pass : ",input.password)
            console.log("User Pass : ",user.password)
            let checkPass = await isSamePass(input.password, user.password);
            console.log("check pass", checkPass);
            if(!checkPass){
            throw new TRPCError({
                code: 'BAD_REQUEST',
                message: 'Password is incorrect'
            });
            }

            return {user};
        } catch (e: any) {
            console.log("data error", e);
                
            throw new TRPCError({
                code: 'INTERNAL_SERVER_ERROR',
                message: e.message
            })
        }
    }),



    register: publicProcedure
    .input(registerSchema)
    .mutation(async ({ input }) => {
        try {   
            const exists = await prisma.user.findFirst({
                where: { email: input.email },
            });

            if (exists) {
                throw new TRPCError({
                    code: 'FORBIDDEN',
                    message: 'User already exists.',
                });
            }
            let hashPassword = hashPass(input.password)
            const paylaod: any = {
                email: input.email,
                password: hashPassword,
                name: input?.name,
                role_id: input?.role_id
            };
            console.log(paylaod, "paylaod");
            const user = await prisma?.user?.create({
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


})