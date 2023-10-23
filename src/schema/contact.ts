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
  number: z
    .string({
      required_error: 'Please enter your number',
    })
    .length(15),
  message: z.string({
    required_error: 'Please write your message',
  }),
});

export const contactSchema = z.object({
  name: z
    .string({ required_error: 'Please enter your name' })
    .min(1, { message: 'Please enter your name' })
    .max(24, {
      message: 'Name must not exceed 30 characters',
    }),
  email: z.string({ required_error: 'Please enter your email' }).email({
    message: "Please enter a valid email"
  }),
  code: z.string({ required_error: 'Enter code' })
    .regex(new RegExp(/^(\+)?[0-9]+$/), 'Invalid code')
    .min(1, {
      message: 'Enter code',
    }).max(4, {
      message: 'Enter code',
    }),
  number: z
    .string({ required_error: 'Please enter your number' })
    .regex(new RegExp(/^[0-9]+$/), 'Please enter a valid  number')
    .min(9, {
      message: 'Number should be at more than 7 characters',
    })
    .max(15, {
      message: 'Number should be at less than 15 characters',
    }),
  message: z
    .string({ required_error: 'Please write your message' })
    .min(1, { message: 'Please write your message' }),
});

export type contactSchemaInput = z.infer<typeof contactSchema>;
