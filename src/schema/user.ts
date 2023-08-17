import { z } from "zod"

export const otpSchema = z.object({
    email: z.string({ required_error: 'Invalid email' }).email(),
    otp: z.string().length(4),
});

export const loginSchema = z.object({
    email: z.string({ required_error: 'Invalid email' }).email(),
});
