import { prisma } from '../prisma';

import { getUserData } from '~/utils/helper';

/* 
 ---- input ----
 email
 password 
*/
export async function getAnalytics(req: any, res: any) {
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

    const userPromise = await prisma.user.findFirst({
      where: {
        id: userData?.id ?? 0,
      },
      include: {
        Role: true,
      },
    });
    console.log({ userData });
    const AnalyticObject: any = {};
    if (userPromise) {
      const options: any = {
        where: {
          created_by: userData?.id,
          is_deleted: false,
        },
      };
      if (userData?.Role?.name == 'trucker') {
        options.where = {
          trucker_id: {
            hasEvery: [userData?.id],
          },
          is_deleted: false,
        };
      }
      if (userData?.Role?.name == 'client') {
        options.where = {
          client_id: userData?.id,
          is_deleted: false,
        };
      }
      if (userData?.Role?.name == 'seller') {
        options.where = {
          created_by: userData?.id,
          is_deleted: false,
        };
      }
      const totalProjects = await prisma.projects.count({
        where: options.where,
      });
      AnalyticObject.projects = totalProjects ?? 0;
      return res.status(200).send({
        data: { projects: totalProjects ?? 0 },
        message: 'project found!',
      });
    }

    return res.status(400).send({
      message: 'user not found!',
    });
  } catch (err: any) {
    res.status(500).send({ message: err.message as string });
  }
}
