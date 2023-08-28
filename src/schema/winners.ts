import { z } from 'zod';


export const getWinnersByIdSchema = z.object({
  customer_id: z.number(),
});
