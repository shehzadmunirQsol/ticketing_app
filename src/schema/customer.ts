import { z } from 'zod';


export const signupCustomerSchema = z.object({
  username: z.string(),
  email: z.string().email(),
  password: z.string(),
  firstname: z.string(),
  lastname: z.string(),
});
export type signupCustomerInput = z.TypeOf<typeof signupCustomerSchema>;


export const loginCustomerSchema = z.object({
    user: z.string(),
    password: z.string()
});
export type loginCustomerInput = z.TypeOf<typeof loginCustomerSchema>;

export const forgotPasswordCustomerSchema = z.object({
    email: z.string().email(),
});
export type forgotPasswordCustomerSchemaInput = z.TypeOf<typeof forgotPasswordCustomerSchema>;

export const resetPasswordCustomerSchema = z.object({
    email: z.string(),
    otp: z.string(),
    password: z.string(),
    confirmPassword: z.string(),
});
export type resetPasswordCustomerSchemaInput = z.TypeOf<typeof resetPasswordCustomerSchema>;

export const verificationOtpCustomerSchema = z.object({
    email:z.string().email(),
    otp_1: z.number(),
    otp_2: z.number(),
    otp_3: z.number(),
    otp_4: z.number(),
});

