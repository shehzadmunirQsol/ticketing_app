import { signJWT } from '~/utils/jwt';
import { registerCustomerSchema } from '~/schema/auth';

import { prisma } from '~/server/prisma';

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
    const { role, ...data } = { ...validate?.data };

    // Hash the password
    const findRole = await prisma.role.findFirst({
      where: {
        name: role,
      },
    });

    // Create a new User instance with the hashed password
    const result = await prisma.user.upsert({
      where: {
        email: validate.data?.email,
      },
      update: {
        ...data,
        role_id: findRole?.id,
        is_registerd: true,
      },
      create: {
        ...data,
        is_registerd: true,
        role_id: findRole?.id,
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
      role: result?.Role?.name,
      id: result.id,
    });
    return res.status(200).send({ customer: result, jwt });
  } catch (err: any) {
    console.log({ err });
    res.status(500).send({ message: err.message as string });
  }
}
