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
    message: 'Please enter a valid email',
  }),
  code: z
    .string({
      required_error: 'Enter code',
    })
    .regex(new RegExp(/^(\+)?[0-9]+$/), 'Invalid code')
    .min(1, {
      message: 'Enter code',
    })
    .max(4, {
      message: 'Invalid code',
    })
    .trim(),
  number: z
    .string({
      required_error: 'Please enter your phone no',
      invalid_type_error: 'Please enter your phone no',
    })
    .regex(new RegExp(/^[0-9]+$/), 'Please enter a valid phone no.')
    .min(9, {
      message: 'Should be at more than 9 numbers',
    })
    .max(15, {
      message: 'Should be at less than 15 numbers',
    })
    .trim(),
  message: z
    .string({ required_error: 'Please write your message' })
    .min(1, { message: 'Please write your message' }),
});

export type contactSchemaInput = z.infer<typeof contactSchema>;
