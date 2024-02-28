import { z } from 'zod';
import { validateEmail } from '~/utils/helper';

// export const projectGetAllSchema = z.object({
//   jwt: z.string(),
// });

export const clientGetSearchSchema = z.object({
  // is_detail: z.string().optional(),

  searchQuery: z.string({
    required_error: 'search query is required',
    invalid_type_error: 'search query is required',
  }),
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
