import { prisma } from '~/server/prisma';

import { getUserData } from '~/utils/helper';

export async function getUserPermision(req: any, res: any) {
  try {
    if (!req.query)
      return res.status(400).send({ message: 'payload not found' });
    const input = { ...req.query };
    delete input.routes;

    // Check if the authorization scheme is Bearer and if the token exists
    const userData: any = await getUserData(req, res);
    if (!userData) {
      return res.status(400).send({
        message: 'You are not authorized to access!',
      });
    }

    const userPermisionPromise = await prisma.user.findFirst({
      where: {
        id: userData?.id ?? 0,
      },
      select: {
        Role: {
          include: {
            RolePermsions: {
              include: {
                Resources: {
                  select: {
                    code: true,
                    name: true,
                  },
                },
              },
            },
          },
        },
      },
    });

    return res
      .status(200)
      .send({ data: userPermisionPromise, message: 'permisions found!' });
  } catch (err: any) {
    res.status(500).send({ message: err.message as string });
  }
}
