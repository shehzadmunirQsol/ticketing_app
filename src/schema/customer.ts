import { z } from 'zod';

export const signupCustomerSchema = z.object({
  username: z
    .string({ required_error: 'Please enter your username' })
    .min(3, {
      message: 'Username must be at least 3 characters',
    })
    .max(24, {
      message: 'Username must not exceed 24 characters',
    }),
  email: z
    .string({
      required_error: 'Please enter your email',
      invalid_type_error: 'Please enter your email',
    })
    .email({
      message: 'Please use a valid email ',
    })
    .refine(
      (val) =>
        val.includes('*')
          ? false
          : true,
      {
        message: 'Please use a valid email ',
      },
    ),
  password: z
    .string({ required_error: 'Please enter your password' })
    .min(6, {
      message: 'Password must be at least 6 characters',
    })
    .max(30, {
      message: 'Password must not exceed 30 characters',
    }),

  firstname: z
    .string({ required_error: 'Please enter your firstname' })
    .min(2, {
      message: 'firstname must be at least 2 characters',
    })
    .max(30, {
      message: 'firstname must not exceed 30 characters',
    }),
  lastname: z.string().optional(),
});
export const signupCustomerSchemaInput = z.object({
  username: z
    .string({ required_error: 'Please enter your username' })
    .min(3, {
      message: 'Username must be at least 3 characters',
    })
    .max(24, {
      message: 'Username must not exceed 24 characters',
    }),
  email: z
    .string({
      required_error: 'Please enter your email',
      invalid_type_error: 'Please enter your email',
    })
    .email(),
  password: z
    .string({ required_error: 'Please enter your password' })
    .min(6, {
      message: 'Password must be at least 6 characters',
    })
    .max(30, {
      message: 'Password must not exceed 30 characters',
    }),
  firstname: z
    .string({ required_error: 'Please enter your firstname' })
    .min(2, {
      message: 'firstname must be at least 2 characters',
    })
    .max(30, {
      message: 'firstname must not exceed 30 characters',
    }),
  lastname: z.string().optional(),
});
export type signupCustomerInput = z.TypeOf<typeof signupCustomerSchemaInput>;

export const getCustomerSchema = z.object({
  startDate: z.date().optional(),
  endDate: z.date().optional(),
  searchQuery: z.string().optional(),
  first: z.number(),
  rows: z.number(),
  filters: z.any().optional(),
});
export const updateCustomerSchema = z.object({
  id: z.number(),
  is_approved: z.boolean().optional(),
  is_deleted: z.boolean().optional(),
});
export type getCustomerSchema = z.TypeOf<typeof getCustomerSchema>;

export const loginCustomerSchema = z.object({
  user: z.string({ required_error: 'Please enter your username' }),
  password: z
    .string({ required_error: 'Please enter your password' })
    .min(6, {
      message: 'Password must be at least 6 characters',
    })
    .max(30, {
      message: 'Password must not exceed 30 characters',
    }),
});
export const loginCustomerSchemaInput = z.object({
  user: z.string({ required_error: 'Please enter your username' }),
  password: z
    .string({ required_error: 'Please enter your password' })
    .min(6, {
      message: 'Password must be at least 6 characters',
    })
    .max(30, {
      message: 'Password must not exceed 30 characters',
    }),
});

export type loginCustomerInput = z.TypeOf<typeof loginCustomerSchemaInput>;

export const forgotPasswordCustomerSchema = z.object({
  email: z
    .string({
      required_error: 'Please enter your email',
      invalid_type_error: 'Please enter your email',
    })
    .email(),
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
  street_address_1: z.string().optional(),
  city: z.string().optional(),
  country: z.string().optional(),
  phone_number: z.string().optional(),
  postal_code: z.number().optional(),
});

export const getCustomerAddress = z.object({
  customer_id: z.number(),
});

export const accountsDetailSchema = z.object({
  first_name: z.string(),
  last_name: z.string(),
  email: z.string().email(),
  dob: z.date().optional().nullable(),
});

export const accountsDetailSchemaInput = z.object({
  first_name: z
    .string()
    .min(1, {
      message: 'Required',
    })
    .max(24, {
      message: 'Name must not exceed 24 characters',
    }),
  last_name: z
    .string()
    .min(1, {
      message: 'Required',
    })
    .max(24, {
      message: 'Name must not exceed 24 characters',
    }),
  email: z.string().email(),
  dob: z.date().optional().nullable(),
});

export type accountsDetailSchemaInput = z.infer<
  typeof accountsDetailSchemaInput
>;

export const passwordChangeSchema = z.object({
  email: z.string().email().optional(),
  currentPassword: z.string(),
  newPassword: z.string(),
  confirmPassword: z.string(),
});

export const passwordChangeSchemaInput = z.object({
  email: z.string().email().optional(),
  currentPassword: z.string(),
  newPassword: z
    .string()
    .min(6, {
      message: 'Password must be at least 6 characters',
    })
    .max(30, {
      message: 'Password must not exceed 30 characters',
    }),
  confirmPassword: z
    .string()
    .min(6, {
      message: 'Confirm Password must be at least 6 characters',
    })
    .max(30, {
      message: 'Confirm Password must not exceed 30 characters',
    }),
});

export type passwordChangeSchemaInput = z.infer<
  typeof passwordChangeSchemaInput
>;

export const deleteMyAccountCustomerSchema = z.object({
  email: z.string().email().optional(),
  message: z.string().optional(),
  reasons: z.array(z.string()).optional(),
});

export const deleteMyAccountCustomerSchemaInput = z.object({
  email: z.string().email().optional(),
  message: z.string().optional(),
  reasons: z.array(z.string()).optional(),
});

export type deleteMyAccountCustomerSchemaInput = z.infer<
  typeof deleteMyAccountCustomerSchemaInput
>;

export type addAddressInput = z.TypeOf<typeof addCustomerAddress>;
export const logoutSchema = z.object({});
