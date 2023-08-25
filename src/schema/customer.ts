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
    email: z.string().email(),
    password: z.string()
});
export type loginCustomerInput = z.TypeOf<typeof loginCustomerSchema>;

export const forgotPasswordCustomerSchema = z.object({
    email: z.string().email(),
});
export type forgotPasswordCustomerSchemaInput = z.TypeOf<typeof forgotPasswordCustomerSchema>;

