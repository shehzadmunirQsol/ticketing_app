import { z } from 'zod';
import { validateEmail } from '~/utils/helper';

export const otpSchema = z.object({
  email: z.string({ required_error: 'Invalid email' }).email(),
  otp: z.string().length(4),
});

export const loginSchema = z.object({
  email: z.string({ required_error: 'Invalid email' }).email(),
  password: z.string(),
});

export const registerSchema = z.object({
  email: z.string({ required_error: 'Invalid email' }).email(),
  name: z
    .string()
    .min(3, 'User name should be equal to or greater than 3 characters.')
    .max(36, 'User name should be equal to or less than 36 characters.'),
  role_id: z.number(),
  password: z.string(),
});

export const logoutSchema = z.object({});

export const updateUserSchema = z.object({
  id: z.number(),
  name: z
    .string()
    .min(2, 'User name should be equal to or greater than 3 characters.')
    .max(36, 'User name should be equal to or less than 36 characters.'),
});

export const deleteUserSchema = z.object({
  id: z.number(),
});

export const getAdminSchema = z.object({
  isLogin: z.boolean(),
});
export const passwordChangeSchema = z.object({
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

export type passwordChangeSchemaInput = z.infer<typeof passwordChangeSchema>;
export const emailChangeSchema = z.object({
  id: z
    .number({
      required_error: 'Id is missing in payload',
      invalid_type_error: 'Id is missing in payload',
    })
    .optional()
    .nullable(),
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
});
const noNameDataSchema = emailChangeSchema.omit({ id: true });

export type emailChangeSchemaInput = z.infer<typeof noNameDataSchema>;
export type LoginInput = z.TypeOf<typeof loginSchema>;
export type RegisterInput = z.TypeOf<typeof registerSchema>;
