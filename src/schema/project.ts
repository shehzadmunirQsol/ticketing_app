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
  name: z.string({
    required_error: 'name required',
    invalid_type_error: 'name required',
  }),
  client_id: z.number({
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
  address: z.array(z.object({})).optional().nullable(),
});
// register schema for api

// send request trucker
