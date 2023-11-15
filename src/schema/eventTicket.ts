import { z } from 'zod';

export const getEventTicketsSchema = z.object({
  event_id: z.number(),
  order_event_id: z.number(),
});
