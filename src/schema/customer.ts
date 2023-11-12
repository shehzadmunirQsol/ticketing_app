import { z } from 'zod';

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
    }),
  password: z
    .string({ required_error: 'Please enter your password' })
    .min(6, {
      message: 'Password must be at least 6 characters',
    })
    .max(30, {
      message: 'Password must not exceed 30 characters',
    }),
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
  is_verified: z.boolean().optional(),
  first: z.number(),
  rows: z.number(),
  filters: z.any().optional(),
});
export const updateCustomerSchema = z.object({
  id: z.number(),
  is_approved: z.boolean().optional(),
  is_disabled: z.boolean().optional(),
  is_deleted: z.boolean().optional(),
  is_blocked: z.boolean().optional(),
  type: z.string().optional(),
});
export type getCustomerSchema = z.TypeOf<typeof getCustomerSchema>;

export const loginCustomerSchema = z.object({
  user: z
    .string({
      required_error: 'Please enter your email',
      invalid_type_error: 'Please enter your email',
    })
    .email({
      message: 'Please use a valid email',
    })
    .trim(),
  password: z
    .string({ required_error: 'Please enter your password' })
    .min(6, {
      message: 'Password must be at least 6 characters',
    })
    .max(30, {
      message: 'Password must not exceed 30 characters',
    })
    .trim(),
});
export const loginCustomerSchemaInput = z.object({
  user: z.string({ required_error: 'Please enter your username' }).trim(),
  password: z
    .string({ required_error: 'Please enter your password' })
    .min(6, {
      message: 'Password must be at least 6 characters',
    })
    .max(30, {
      message: 'Password must not exceed 30 characters',
    })
    .trim(),
});

export type loginCustomerInput = z.TypeOf<typeof loginCustomerSchema>;

export const forgotPasswordCustomerSchema = z.object({
  email: z
    .string({
      required_error: 'Please enter your email',
      invalid_type_error: 'Please enter your email',
    })
    .email()
    .trim(),
});
export type forgotPasswordCustomerSchemaInput = z.TypeOf<
  typeof forgotPasswordCustomerSchema
>;

export const resetPasswordCustomerSchema = z.object({
  email: z.string().trim(),
  otp: z.string().trim(),
  password: z.string().trim(),
  confirmPassword: z.string().trim(),
});
export type resetPasswordCustomerSchemaInput = z.TypeOf<
  typeof resetPasswordCustomerSchema
>;

export const verificationOtpCustomerSchema = z.object({
  emailOrUser: z.string(),
  otp_1: z.number(),
  otp_2: z.number(),
  otp_3: z.number(),
  otp_4: z.number(),
});

export const resendOtpCustomerSchema = z.object({
  emailOrUser: z.string(),
});

export const addCustomerAddress = z.object({
  id: z.number().optional(),
  customer_id: z.number(),
  address_type: z
    .enum(['home', 'work', 'hotel', 'other'], {
      required_error: 'Please select billing address',
    })
    .default('home'),
  street_address_1: z
    .string({
      required_error: 'Please enter your street address',
      invalid_type_error: 'Please enter your street address',
    })
    .min(1, {
      message: 'Please enter your street address',
    })
    .trim(),
  street_address_2: z
    .string({
      required_error: 'Please enter your appartment',
      invalid_type_error: 'Please enter your appartment',
    })
    .trim()
    .optional(),
  state: z
    .string({
      required_error: 'Please enter your state',
      invalid_type_error: 'Please enter your state',
    })
    .min(1, {
      message: 'Please enter your state',
    })
    .trim(),
  city: z
    .string({
      required_error: 'Please enter your city',
      invalid_type_error: 'Please enter your city',
    })
    .min(1, {
      message: 'Please enter your city',
    })
    .trim(),
  country: z
    .string({
      required_error: 'Please enter your country',
      invalid_type_error: 'Please enter your country',
    })
    .min(1, {
      message: 'Please enter your country',
    })
    .trim(),
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
  phone_code: z
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
  postal_code: z
  .string()
  .optional(),
});
export const updateCustomerAddress = z.object({
  id: z.number().optional(),
  customer_id: z.number(),
});
export const getCustomerAddress = z.object({
  customer_id: z.number(),
});

export const accountsDetailSchema = z.object({
  id: z.number().optional(),
  first_name: z.string(),
  last_name: z.string(),
  dob: z.date().optional().nullable(),
});

export const accountsDetailSchemaInput = z.object({
  first_name: z
    .string()
    .min(1, {
      message: 'Please enter your first name',
    })
    .max(24, {
      message: 'Name must not exceed 24 characters',
    })
    .trim(),
  last_name: z
    .string()
    .min(1, {
      message: 'Please enter your last name',
    })
    .max(24, {
      message: 'Name must not exceed 24 characters',
    })
    .trim(),
  dob: z
    .date()
    .refine((d) => d >= new Date('1900-01-01') && d <= new Date('2100-01-01'), {
      message: 'Enter a valid date',
    })
    .optional()
    .nullable(),
});

export type accountsDetailSchemaInput = z.infer<
  typeof accountsDetailSchemaInput
>;

export const passwordChangeSchema = z.object({
  email: z.string().trim().email().optional(),
  currentPassword: z.string().trim(),
  newPassword: z.string().trim(),
  confirmPassword: z.string().trim(),
});

export const passwordChangeSchemaInput = z.object({
  email: z.string().trim().email().optional(),
  currentPassword: z.string().trim().min(6, {
    message: 'Current Password must be at least 6 characters',
  }),
  newPassword: z
    .string()
    .min(6, {
      message: 'New Password must be at least 6 characters',
    })
    .max(30, {
      message: 'New Password must not exceed 30 characters',
    })
    .trim(),
  confirmPassword: z
    .string()
    .min(6, {
      message: 'Confirm Password must be at least 6 characters',
    })
    .max(30, {
      message: 'Confirm Password must not exceed 30 characters',
    })
    .trim(),
});

export type passwordChangeSchemaInput = z.infer<
  typeof passwordChangeSchemaInput
>;

export const deleteMyAccountCustomerSchema = z.object({
  email: z.string().trim().email().optional(),
  message: z.string().trim().optional(),
  reasons: z.array(z.string()).optional(),
});

export const deleteMyAccountCustomerSchemaInput = z.object({
  email: z.string().trim().email().optional(),
  message: z.string().trim().optional(),
  reasons: z.array(z.string()).optional(),
});

export type deleteMyAccountCustomerSchemaInput = z.infer<
  typeof deleteMyAccountCustomerSchemaInput
>;

export type addAddressInput = z.TypeOf<typeof addCustomerAddress>;
export const logoutSchema = z.object({});
