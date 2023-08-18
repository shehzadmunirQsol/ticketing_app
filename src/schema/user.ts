import { z } from "zod"

export const otpSchema = z.object({
    email: z.string({ required_error: 'Invalid email' }).email(),
    otp: z.string().length(4),
});


export const loginSchema = z.object({
    email: z.string({ required_error: 'Invalid email' }).email(),
    password: z.string()
});


export const registerSchema = z.object({
    email: z.string({ required_error: 'Invalid email' }).email(),
    name: z
      .string()
      .min(3, 'User name should be equal to or greater than 3 characters.')
      .max(36, 'User name should be equal to or less than 36 characters.'),
    role_id: z.number(),
    password: z.string()
});

export const logoutSchema = z.object({});

export const updateUserSchema= z.object({
    id: z.number(),
    name: z
        .string()
        .min(3, 'User name should be equal to or greater than 3 characters.')
        .max(36, 'User name should be equal to or less than 36 characters.'),
});

export const deleteUserSchema = z.object({
    id: z.number(),
});

export type LoginInput = z.TypeOf<typeof loginSchema>;
export type RegisterInput = z.TypeOf<typeof registerSchema>;