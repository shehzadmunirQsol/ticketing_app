import { z } from 'zod';

export const getEventTicketsSchema = z.object({
  event_id: z.number(),
  order_event_id: z.number(),
});
export const getEventTicketCustomerSchema = z.object({
  event_id: z.number(),
  ticket_num: z.number(),
});
export const getAllEventTicketCustomersSchema = z.object({
  event_id: z.number(),
});
