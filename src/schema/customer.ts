import { z } from 'zod';
import { validateEmail } from '~/utils/helper';

export const signupCustomerSchema = z.object({
  email: z
    .string({
      required_error: 'Please enter your email',
      invalid_type_error: 'Please enter your email',
    })
    .email({
      message: 'Please use a valid email ',
    })
    .refine((val) => (val.includes('*') ? false : true), {
      message: 'Please use a valid email ',
    }),
  password: z
    .string({ required_error: 'Please enter your password' })
    .min(6, {
      message: 'Password must be at least 6 characters',
    })
    .max(30, {
      message: 'Password must not exceed 30 characters',
    }),
  confirmpassword: z.string({ required_error: 'Please enter your password' }),
  first_name: z
    .string({ required_error: 'Please enter your firstname' })
    .min(2, {
      message: 'firstname must be at least 2 characters',
    })
    .max(30, {
      message: 'firstname must not exceed 30 characters',
    }),
  last_name: z.string().optional(),
  code: z.string(),
  phone_number: z
    .string()
    .regex(new RegExp(/^[0-9]+$/), 'Please enter a valid phone number')
    .min(1, {
      message: 'Please enter your number',
    }),
  country: z.string({
    required_error: 'Please select your country',
  }),
  dob: z.date(),
  gender: z.enum(['male', 'female'], {
    required_error: 'Please enter your gender',
  }),
});
export const signupCustomerSchemaInput = z.object({
  email: z
    .string({
      required_error: 'Please enter your email',
      invalid_type_error: 'Please enter your email',
    })
    .email({
      message: 'Please enter your email',
    })
    .refine((val) => (val.includes('*') ? false : true), {
      message: 'Please use a valid email ',
    })

    .refine((val) => (val.includes('-') ? false : true), {
      message: 'Please use a valid email ',
    })
    .refine((val) => validateEmail(val), {
      message: 'Invalid email format.',
    }),
  password: z
    .string({ required_error: 'Please enter your password' })
    .min(6, {
      message: 'Password must be at least 6 characters',
    })
    .max(30, {
      message: 'Password must not exceed 30 characters',
    }),
  confirmpassword: z.string({ required_error: 'Please enter your password' }),
  first_name: z
    .string({ required_error: 'Please enter your First Name' })
    .min(2, {
      message: 'First Name must be at least 2 characters',
    })
    .max(30, {
      message: 'First Name must not exceed 30 characters',
    }),
  last_name: z
    .string({ required_error: 'Please enter your Last Name' })
    .min(2, {
      message: 'Last Name must be at least 2 characters',
    })
    .max(30, {
      message: 'Last Name must not exceed 30 characters',
    }),
  phone_number: z
    .string({
      required_error: 'Please enter your phone no',
      invalid_type_error: 'Please enter your phone no',
    })
    .regex(new RegExp(/^[0-9]+$/), 'Please enter a valid phone no.')
    .min(9, {
      message: 'Should be at more than 9 numbers',
    })
    .max(15, {
      message: 'Should be at less than 15 numbers',
    })
    .trim(),
  code: z
    .string({
      required_error: 'Enter code',
    })
    .regex(new RegExp(/^(\+)?[0-9]+$/), 'Invalid code')
    .min(1, {
      message: 'Enter code',
    })
    .max(4, {
      message: 'Invalid code',
    })
    .trim(),

  dob: z.date(),
  country: z
    .string({
      required_error: 'Please select your country',
    })
    .min(1, {
      message: 'Please select your country',
    }),
  gender: z.enum(['male', 'female'], {
    required_error: 'Please enter your gender',
  }),
});
export type signupCustomerInput = z.TypeOf<typeof signupCustomerSchemaInput>;

export const getCustomerSchema = z.object({
  startDate: z.date().optional(),
  endDate: z.date().optional(),
  searchQuery: z.string().optional(),
  role_id: z.number(),
  first: z.number(),
  rows: z.number(),
  filters: z.any().optional(),
});
export const getCustomerDetailSchema = z.object({
  startDate: z.date().optional(),
  endDate: z.date().optional(),
  customer_id: z.number(),
  first: z.number(),
  rows: z.number(),
  lang_id: z.number().optional(),
  filters: z.any().optional(),
  status: z.string().optional(),
});
export const updateCustomerSchema = z.object({
  id: z.number(),
  is_approved: z.boolean().optional(),
  is_disabled: z.boolean().optional(),
  is_deleted: z.boolean().optional(),
  is_blocked: z.boolean().optional(),
  type: z.string().optional(),
});
export type getCustomerFilterSchema = z.TypeOf<typeof getCustomerSchema>;
export const createUserSchema = z.object({
  first_name: z
    .string()
    .min(2, 'User name should be equal to or greater than 3 characters.')
    .max(36, 'User name should be equal to or less than 36 characters.'),
  email: z
    .string({
      required_error: 'Please enter your email',
      invalid_type_error: 'Please enter your email',
    })
    .email({
      message: 'Please enter your email',
    })
    .refine((val) => (val.includes('*') ? false : true), {
      message: 'Please use a valid email ',
    })

    .refine((val) => (val.includes('-') ? false : true), {
      message: 'Please use a valid email ',
    })
    .refine((val) => validateEmail(val), {
      message: 'Invalid email format.',
    }),
  type: z
    .string({
      required_error: 'Please enter your type',
      invalid_type_error: 'Please enter your type',
    })
    .min(1, {
      message: 'Invalid type',
    }),
});
