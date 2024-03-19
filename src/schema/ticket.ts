import { z } from 'zod';
import { validateEmail } from '~/utils/helper';

export const ticketCreateSchema = z.object({
  project_id: z.number({
    required_error: 'project_id required',
    invalid_type_error: 'project_id required',
  }),
  private_address: z.string({
    required_error: 'Please provide wallet address',
  }),
  comments: z.string({
    required_error: 'comments required',
    invalid_type_error: 'namecomments required',
  }),
  signature: z.string({
    required_error: 'signature required',
    invalid_type_error: 'signature required',
  }),
});
