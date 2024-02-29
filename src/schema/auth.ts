import { z } from 'zod';
import { validateEmail } from '~/utils/helper';

export const loginCustomerSchema = z.object({
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

  wallet_address: z.string({ required_error: 'Please provide wallet address' }),
  first_name: z.string({ required_error: 'Please provide name' }),
});

// register schema for api
export const registerCustomerSchema = z.object({
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
  phone_number: z
    .string()
    .regex(new RegExp(/^[0-9]+$/), 'Please enter a valid phone number')
    .min(1, {
      message: 'Please enter your number',
    }),
  username: z
    .string({ required_error: 'Please enter your username' })
    .min(1, {
      message: 'username must be at least 6 characters',
    })
    .max(30, {
      message: 'username must not exceed 30 characters',
    }),
  role: z.enum(['seller', 'buyer', 'trucker', 'client'], {
    required_error: 'Please enter select role',
  }),
  wallet_address: z.string({ required_error: 'Please provide wallet address' }),
});

// send request trucker
