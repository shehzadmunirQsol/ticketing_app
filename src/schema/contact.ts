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
    .length(9),
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
  code: z.string({
    required_error: 'Please select your code',
  }).regex(new RegExp(/^[0-9]+$/), 'Please enter a valid phone code')
  .min(1, {
    message: 'Please enter your phone code',
  }),
  number: z
    .string({ required_error: 'Please enter your number' })
    .regex(new RegExp(/^[0-9]+$/), 'Please enter a valid phone number')
    .min(1, {
      message: 'Please enter your number',
    }),
  message: z
    .string({ required_error: 'Please write your message' })
    .min(1, { message: 'Please write your message' }),
});

export type contactSchemaInput = z.infer<typeof contactSchema>;
