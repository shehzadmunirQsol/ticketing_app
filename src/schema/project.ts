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

  searchQuery: z.string().optional().nullable(),
  startDate: z.string().optional().nullable(),
  endDate: z.string().optional().nullable(),
  is_listed: z.boolean().optional().nullable(),
  sell_type: z.string().optional().nullable(),
  is_lazy: z.boolean().optional().nullable(),
});

export const projectCreateSchema = z.object({
  name: z.string({
    required_error: 'name required',
    invalid_type_error: 'name required',
  }),

  desc: z
    .string({
      required_error: 'description required',
      invalid_type_error: 'description required',
    })
    .optional()
    .nullable(),
  price: z.number({
    required_error: 'price required',
    invalid_type_error: 'price required',
  }),
  total_rounds: z.number({
    required_error: 'Total rounds required',
    invalid_type_error: 'Total rounds required',
  }),
  material_type: z.enum(['dump', 'sand'], {
    required_error: 'please select material type',
  }),
  client: z.object({
    first_name: z.string({
      required_error: 'client name required',
      invalid_type_error: 'client name required',
    }),
    email: z
      .string({
        required_error: 'client email required',
        invalid_type_error: 'client email required',
      })
      .email({
        message: 'client email required',
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
  }),
  address: z.array(
    z.object({
      address_type: z.enum(['pick', 'drop'], {
        required_error: ' please select address type',
      }),
      longitude: z.string({
        required_error: 'Address longitude required',
        invalid_type_error: 'Address longitude required',
      }),
      latitude: z.string({
        required_error: 'Address latitude required',
        invalid_type_error: 'Address latitude required',
      }),
      street_address_1: z
        .string({
          required_error: 'Street Address 1 is required',
          invalid_type_error: 'Street Address 1 is required',
        })
        .optional()
        .nullable(),
      street_address_2: z
        .string({
          required_error: 'Street Address 2 is required',
          invalid_type_error: 'Street Address 2 is required',
        })
        .optional()
        .nullable(),
    }),
  ),
});

export const inviteTruckerSchema = z.object({
  first_name: z.string({
    required_error: 'client name required',
    invalid_type_error: 'client name required',
  }),
  email: z
    .string({
      required_error: 'client email required',
      invalid_type_error: 'client email required',
    })
    .email({
      message: 'client email required',
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
  type: z
    .enum(['seller', 'buyer', 'client', 'trucker'], {
      required_error: 'Please enter your type',
      invalid_type_error: 'Please enter your type',
    })
    .default('seller'),
});
// register schema for api

// send request trucker
