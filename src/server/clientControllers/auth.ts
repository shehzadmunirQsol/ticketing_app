import { signJWT } from '~/utils/jwt';
import { prisma } from '../prisma';
import { loginCustomerSchema, registerCustomerSchema } from '~/schema/auth';
import bcrypt from 'bcryptjs';
import { Prisma } from '@prisma/client';

/* 
 ---- input ----
 email
 password 
*/
export async function loginCustomer(req: any, res: any) {
  // const input = req.body;

  try {
    if (!req.body)
      return res.status(400).send({ message: 'payload not found' });

    const input = req.body;
    // const input = JSON.parse(req.body as any);
    const validate = loginCustomerSchema.safeParse(input);

    if (!validate.success)
      return res.status(400).send({
        message:
          validate?.error && validate?.error?.errors[0]?.message
            ? validate?.error?.errors[0]?.message
            : 'Bad Request',
      });
    let customer: any;
    customer = await prisma.user.findFirst({
      where: {
        email: validate.data?.email,
      },
    });

    if (!customer) {
      customer = await prisma.user.create({
        data: {
          ...validate.data,
        },
      });

      return res.status(201).send({ customer, is_registered: false });
    }
    if (!customer?.wallet_address && validate.data?.wallet_address) {
      customer = await prisma.user.update({
        where: {
          id: customer.id,
        },
        data: {
          wallet_address: validate.data?.wallet_address,
        },
      });
    }
    if (!customer?.is_registered && !customer?.role) {
      return res.status(201).send({ customer, is_registered: false });
    }
    const jwt = signJWT({
      email: customer.email,
      role: customer.role,
      id: customer.id,
    });
    const { password, ...data } = customer;
    return res.status(200).send({ customer: data, jwt });
  } catch (err: any) {
    res.status(500).send({ message: err.message as string });
  }
}
export async function registerCustomer(req: any, res: any) {
  // const input = req.body;

  try {
    if (!req.body)
      return res.status(400).send({ message: 'payload not found' });

    const input = req.body;
    const validate = registerCustomerSchema.safeParse(input);

    if (!validate.success)
      return res.status(400).send({
        message:
          validate?.error && validate?.error?.errors[0]?.message
            ? validate?.error?.errors[0]?.message
            : 'Bad Request',
      });

    // const existingUser = await User.findOne({ email });
    const existingUser = await prisma.user.findFirst({
      where: { email: validate.data?.email },
    });

    if (existingUser?.is_registerd || existingUser?.role) {
      return res.status(400).json({
        error: 'Email already exists. Please use a different email.',
      });
    }

    // Hash the password

    // Create a new User instance with the hashed password
    const result = await prisma.user.upsert({
      where: {
        email: validate.data?.email,
      },
      update: {
        ...validate.data,
      },
      create: {
        ...validate.data,
      },
    });
    const jwt = signJWT({
      email: result.email,
      role: result.role,
      id: result.id,
    });
    return res.status(200).send({ customer: result, jwt });
  } catch (err: any) {
    console.log({ err });
    res.status(500).send({ message: err.message as string });
  }
}
