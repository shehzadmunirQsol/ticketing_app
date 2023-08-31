import { z } from 'zod';

export const signupCustomerSchema = z.object({
  username: z.string(),
  email: z.string().email(),
  password: z.string(),
  firstname: z.string(),
  lastname: z.string(),
});
export type signupCustomerInput = z.TypeOf<typeof signupCustomerSchema>;
export const getCustomerSchema = z.object({
  startDate: z.date().optional(),
  endDate: z.date().optional(),
  searchQuery: z.string().optional(),
  first: z.number(),
  rows: z.number(),
});
export const updateCustomerSchema = z.object({
  id: z.number(),
  is_approved: z.boolean().optional(),
  is_deleted: z.boolean().optional(),
});
export type getCustomerSchema = z.TypeOf<typeof getCustomerSchema>;

export const loginCustomerSchema = z.object({
    user: z.string(),
    password: z.string()
});
export type loginCustomerInput = z.TypeOf<typeof loginCustomerSchema>;

export const forgotPasswordCustomerSchema = z.object({
  email: z.string().email(),
});
export type forgotPasswordCustomerSchemaInput = z.TypeOf<
  typeof forgotPasswordCustomerSchema
>;

export const resetPasswordCustomerSchema = z.object({
  email: z.string(),
  otp: z.string(),
  password: z.string(),
  confirmPassword: z.string(),
});
export type resetPasswordCustomerSchemaInput = z.TypeOf<
  typeof resetPasswordCustomerSchema
>;
