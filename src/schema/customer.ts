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
  phone_number: z.string().regex(new RegExp(/^[0-9]+$/), 'Please enter a valid phone number')
    .min(1, {
      message: 'Please enter your number',
    }),
  country: z.string({
    required_error: "Please select your country"
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
    .string({ required_error: 'Please enter your firstname' })
    .min(2, {
      message: 'firstname must be at least 2 characters',
    })
    .max(30, {
      message: 'firstname must not exceed 30 characters',
    }),
  last_name: z
    .string({ required_error: 'Please enter your lastname' })
    .min(2, {
      message: 'lastname must be at least 2 characters',
    })
    .max(30, {
      message: 'lastname must not exceed 30 characters',
    }),
  code: z.string(),
  phone_number: z
    .string({
      required_error: 'Please enter your phone no',
      invalid_type_error: 'Please enter your phone no',
    })
    .min(1, {
      message: 'Please enter phone no',
    }),
  dob: z.date(),
  country: z.string({
    required_error: "Please select your country"
  }).min(1, {
    message: "Please select your country"
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
    .refine((val) => (val.includes('*') ? false : true), {
      message: 'Please use a valid email',
    }),
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

export type loginCustomerInput = z.TypeOf<typeof loginCustomerSchema>;

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
  street_address_1: z.string({
    required_error: 'Please enter your street address',
    invalid_type_error: 'Please enter your street address',
  }).trim(),
  city: z.string({
    required_error: 'Please enter your city',
    invalid_type_error: 'Please enter your city',
  }).trim(),
  country: z.string({
    required_error: 'Please enter your country',
    invalid_type_error: 'Please enter your country',
  }).trim(),
  phone_number: z.string({
    required_error: 'Please enter your phone no',
    invalid_type_error: 'Please enter your phone no',
  }).trim(),
  postal_code: z.number({
    required_error: 'Please enter your postal code',
    invalid_type_error: 'Please enter your postal code',
  }),
});

export const getCustomerAddress = z.object({
  customer_id: z.number(),
});

export const accountsDetailSchema = z.object({
  first_name: z.string(),
  last_name: z.string(),
  email: z.string().email(),
  dob: z.date().optional().nullable(),
  country:z.string().optional(),
});

export const accountsDetailSchemaInput = z.object({
  first_name: z
    .string()
    .min(1, {
      message: 'Please enter your first name',
    })
    .max(24, {
      message: 'Name must not exceed 24 characters',
    }).trim(),
  last_name: z
    .string()
    .min(1, {
      message: 'Please enter your last name',
    })
    .max(24, {
      message: 'Name must not exceed 24 characters',
    }).trim(),
  email: z.string().trim().email(),
  dob: z.date().optional().nullable(),
  country: z.string({
    required_error: "Please select your country"
  }).min(1, {
    message: "Please select your country"
  }),
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
  currentPassword: z.string().trim(),
  newPassword: z
    .string()
    .min(6, {
      message: 'Password must be at least 6 characters',
    })
    .max(30, {
      message: 'Password must not exceed 30 characters',
    }).trim(),
  confirmPassword: z
    .string()
    .min(6, {
      message: 'Confirm Password must be at least 6 characters',
    })
    .max(30, {
      message: 'Confirm Password must not exceed 30 characters',
    }).trim(),
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
