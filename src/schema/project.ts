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
});
export const projectGetSchema = z.object({
  orderBy: z.string().default('desc'),
  // is_detail: z.string().optional(),
  is_archive: z.string().default('false'),

  first: z.string().optional(),
  rows: z.string().optional(),

  searchQuery: z.string().optional().nullable(),
  startDate: z.string().optional().nullable(),
  endDate: z.string().optional().nullable(),
});
export const projectViewSchema = z.object({
  project_id: z.string({
    required_error: 'project ID required',
    invalid_type_error: 'project ID required',
  }),
});
export const projectGetAdminchema = z.object({
  startDate: z.date().optional(),
  endDate: z.date().optional(),
  searchQuery: z.string().optional(),
  type: z.string().optional().nullable(),
  id: z.number().optional().nullable(),
  first: z.number(),
  rows: z.number(),
  filters: z.any().optional(),
});

export const projectGetDetailSchema = z.object({
  id: z.number({
    required_error: 'id is required',
    invalid_type_error: 'id is required',
  }),
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
  total_rounds: z
    .number({
      required_error: 'Total rounds required',
      invalid_type_error: 'Total rounds required',
    })
    .min(1, {
      message: 'Total rounds must be greater than zero.',
    }),
  // private_address: z.string({
  //   required_error: 'Please provide wallet address',
  // }),
  material_type: z.enum(['dump', 'sand'], {
    required_error: 'please select material type',
  }),
  truck_cap: z.enum(['half', 'full'], {
    required_error: 'please select truck capacity',
  }),
  start_date: z
    .string({
      invalid_type_error: 'Please select a start date',
      required_error: 'Please select a start date',
    })
    .min(1, {
      message: 'Please select a start date',
    }),
  delivery_date: z
    .string({
      invalid_type_error: 'Please select a delivery date',
      required_error: 'Please select a delivery date',
    })
    .min(1, {
      message: 'Please select a delivery date',
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
  truckers: z
    .array(
      z.object({
        trucker_id: z.number({
          required_error: 'please select address type',
        }),
      }),
    )
    .min(1, {
      message: 'Please select atleast one trucker',
    }),
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
    .enum(['seller_buyer', 'seller_trucker', 'client', 'trucker'], {
      required_error: 'Please enter your type',
      invalid_type_error: 'Please enter your type',
    })
    .default('seller_buyer'),
});
export const updateProjectTrcuker = z.object({
  project_id: z.number({ required_error: 'Project ID is Missing' }),
  truckers: z
    .array(
      z.object({
        trucker_id: z.number({
          required_error: 'trucker id is missing',
          invalid_type_error: 'Street Address 2 is required',
        }),
      }),
    )
    .min(1, {
      message: 'Please select atleast one trucker',
    }),
});
export const updateProjectClient = z.object({
  project_id: z.number({ required_error: 'Project ID is Missing' }),
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
  address: z.object({
    address_type: z.enum(['drop'], {
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
});
// register schema for api

// send request trucker
