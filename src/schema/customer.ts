import { z } from 'zod';

export const signupCustomerSchema = z.object({
  username: z.string(),
  email: z.string().email(),
  password: z.string(),
  firstname: z.string(),
  lastname: z.string(),
});
export const signupCustomerSchemaInput = z.object({
  username: z
    .string()
    .min(2, {
      message: 'Username must be at least 2 characters',
    })
    .max(24, {
      message: 'Username must not exceed 24 characters',
    }),
  email: z.string().email(),
  password: z
    .string()
    .min(6, {
      message: 'Password must be at least 6 characters',
    })
    .max(30, {
      message: 'Password must not exceed 30 characters',
    }),
  firstname: z.string(),
  lastname: z.string(),
});
export type signupCustomerInput = z.TypeOf<typeof signupCustomerSchemaInput>;

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
  password: z.string(),
});
export const loginCustomerSchemaInput = z.object({
  user: z.string(),
  password: z.string(),
});

export type loginCustomerInput = z.TypeOf<typeof loginCustomerSchemaInput>;

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

export const verificationOtpCustomerSchema = z.object({
  email: z.string().email(),
  otp_1: z.number(),
  otp_2: z.number(),
  otp_3: z.number(),
  otp_4: z.number(),
});

export const resendOtpCustomerSchema = z.object({
  email: z.string().email(),
});


export const addCustomerAddress = z.object({
  customer_id: z.number().optional(),
  name: z.string().optional(),
  street_address_1: z.string().optional(),
  city: z.string().optional(),
  country: z.string().optional(),
  phone_number: z.string().optional(),
  postal_code: z.number().optional(),
})

export const getCustomerAddress = z.object({
  customer_id: z.number(),
})
