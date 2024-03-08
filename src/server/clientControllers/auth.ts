import { signJWT, verifyJWT } from '~/utils/jwt';
import { prisma } from '../prisma';
import { loginCustomerSchema, registerCustomerSchema } from '~/schema/auth';
import bcrypt from 'bcryptjs';
import { Prisma } from '@prisma/client';
import { createSmartAccount } from './web3-controller/createAccount';
// import createSmartAccount fro
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
    const { private_address, ...inputData } = validate.data;
    const decodePrivateAddress: any = await verifyJWT(private_address);
    const smartAccount = await createSmartAccount({
      private_address: decodePrivateAddress.address,
    });
    const smartAccountAddress = await smartAccount.getAccountAddress();
    if (!customer) {
      customer = await prisma.user.create({
        data: {
          ...inputData,
          wallet_address: smartAccountAddress,
        },
      });

      return res.status(201).send({ customer, is_registerd: false });
    }

    //  console.log('address : ', smartAccountAddress);
    if (!customer?.wallet_address && smartAccountAddress) {
      customer = await prisma.user.update({
        where: {
          id: customer.id,
        },
        data: {
          wallet_address: smartAccountAddress,
        },
      });
    }
    if (!customer?.is_registerd && !customer?.role_id) {
      return res
        .status(201)
        .send({ customer, is_registerd: customer?.is_registerd });
    }
    const jwt = signJWT({
      email: customer.email,
      role_id: customer.role_id,
      first_name: customer.first_name,

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

    if (existingUser?.is_registerd || existingUser?.role_id) {
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
        is_registerd: true,
      },
      create: {
        ...validate.data,
        is_registerd: true,
      },
      include: {
        Role: {
          include: {
            RolePermsions: {
              include: {
                Resources: true,
              },
            },
          },
        },
      },
    });
    const jwt = signJWT({
      email: result.email,
      first_name: result.first_name,
      role_id: result.role_id,
      id: result.id,
    });
    return res.status(200).send({ customer: result, jwt });
  } catch (err: any) {
    console.log({ err });
    res.status(500).send({ message: err.message as string });
  }
}
