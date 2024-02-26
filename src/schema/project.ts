import { z } from 'zod';
import { validateEmail } from '~/utils/helper';

// export const projectGetAllSchema = z.object({
//   jwt: z.string(),
// });

export const projectGetAllSchema = z.object({
  orderBy: z.string().default('desc'),
  // is_detail: z.string().optional(),

  first: z.string().optional(),
  rows: z.string().optional(),
  role: z.enum(['seller', 'buyer', 'trucker', 'client'], {
    required_error: 'Please enter your please select type',
  }),
  filters: z
    .object({
      searchQuery: z.string().optional().nullable(),
      startDate: z.string().optional().nullable(),
      endDate: z.string().optional().nullable(),
      is_listed: z.boolean().optional().nullable(),
      sell_type: z.string().optional().nullable(),
      is_lazy: z.boolean().optional().nullable(),
    })
    .optional()
    .nullable(),
});

export const projectCreateSchema = z.object({
  jwt: z.string({
    required_error: 'JWT token required',
    invalid_type_error: 'JWT token required',
  }),
  name: z.string({
    required_error: 'name required',
    invalid_type_error: 'name required',
  }),
  client_id: z.string({
    required_error: 'Client is  required',
    invalid_type_error: 'Client required',
  }),
  desc: z.string({
    required_error: 'description required',
    invalid_type_error: 'description required',
  }),
  price: z.number({
    required_error: 'price required',
    invalid_type_error: 'price required',
  }),
  total_rounds: z.number({
    required_error: 'Total rounds required',
    invalid_type_error: 'Total rounds required',
  }),
  material_type: z.enum(['dump', 'sand'], {
    required_error: 'Please enter your please select material type',
  }),
  address: z.array(z.object({})),
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
    .string({ required_error: 'Please enter your password' })
    .min(1, {
      message: 'Password must be at least 6 characters',
    })
    .max(30, {
      message: 'Password must not exceed 30 characters',
    }),
  role: z.enum(['seller', 'buyer', 'trucker', 'client'], {
    required_error: 'Please enter your please select type',
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

// send request trucker
