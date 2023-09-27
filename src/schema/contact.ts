import { z } from 'zod';

export const contactUsSchema = z.object({
  name: z.string({
    required_error: 'Please enter your name',
  }),
  email: z
    .string({
      required_error: 'Please enter your email',
    })
    .email(),
  code: z.string({
    required_error: 'Please select your code',
  }),
  number: z.string({
    required_error: 'Please enter your number',
  }),
  message: z.string({
    required_error: 'Please write your message',
  }),
});

export const contactSchema = z.object({
  name: z
    .string({ required_error: 'Please enter your name' })
    .max(24, {
      message: 'Name must not exceed 30 characters',
    }),
  email: z.string({ required_error: 'Please enter your email' }).email(),
  code: z.string({
    required_error: 'Please select your code',
  }),
  number: z
    .string({ required_error: 'Please enter your number' })
    .min(9, {
      message: 'Number must be at least 9 characters',
    })
    .max(9, {
      message: 'Number must not exceed 9 characters',
    }),
  message: z.string({ required_error: 'Please write your message' }),
});

export type contactSchemaInput = z.infer<typeof contactSchema>;
