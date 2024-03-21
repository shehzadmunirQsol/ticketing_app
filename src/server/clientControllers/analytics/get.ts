import { prisma } from '~/server/prisma';
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
    const AnalyticObject: any = {};
    if (userData) {
      const options: any = {
        where: {
          created_by: userData?.id,
          is_deleted: false,
        },
      };
      if (userPromise?.Role?.name == 'seller_trucker') {
        options.where = {
          OR: [
            {
              ProjectTruckers: {
                every: {
                  trucker_id: userData?.id,
                },
              },
            },
            {
              created_by: userData?.id,
            },
          ],
          is_deleted: false,
        };
      } else if (userPromise?.Role?.name == 'trucker') {
        options.where = {
          ProjectTruckers: {
            every: {
              trucker_id: userData?.id,
            },
          },
          is_deleted: false,
        };
      } else if (userPromise?.Role?.name == 'client') {
        options.where = {
          client_id: userData?.id,
          is_deleted: false,
        };
      } else if (userPromise?.Role?.name == 'seller_buyer') {
        options.where = {
          created_by: userData?.id,
          is_deleted: false,
        };
      }
      // for user assigned and created project
      const totalProjects = await prisma.projects.count({
        where: options.where,
      });

      const totalTruckers = await prisma.projects.findMany({
        where: options.where,
        select: {
          _count: {
            select: {
              ProjectTruckers: true,
            },
          },
        },
      });
      // if user is seller
      if (
        userPromise?.Role?.name == 'seller_buyer' ||
        userPromise?.Role?.name == 'seller_trucker'
      ) {
        const pendingTicketsPromise = await prisma.projects.findMany({
          where: {
            ...options.where,
            ProjectTickets: {
              every: {
                status: 'pending',
              },
            },
          },
          select: {
            _count: {
              select: {
                ProjectTickets: true,
              },
            },
          },
        });
        const completedTicketsPromise = await prisma.projects.findMany({
          where: {
            ...options.where,
            ProjectTickets: {
              every: {
                status: 'completed',
              },
            },
          },
          select: {
            _count: {
              select: {
                ProjectTickets: true,
              },
            },
          },
        });
        let TrcukerSum = 0;
        let PendingTicket = 0;
        let CompletedTicket = 0;
        pendingTicketsPromise.forEach((tickets) => {
          PendingTicket += tickets._count.ProjectTickets;
        });
        completedTicketsPromise.forEach((tickets) => {
          CompletedTicket += tickets._count.ProjectTickets;
        });
        totalTruckers.forEach((trucker) => {
          TrcukerSum += trucker._count.ProjectTruckers;
        });
        AnalyticObject.truckers = TrcukerSum ?? 0;
        AnalyticObject.pendingTickets = PendingTicket ?? 0;
        AnalyticObject.completedTickets = CompletedTicket ?? 0;
      }
      AnalyticObject.projects = totalProjects ?? 0;
      return res.status(200).send({
        data: { ...AnalyticObject },
        message: 'data found!',
      });
    }

    return res.status(400).send({
      message: 'user not found!',
    });
  } catch (err: any) {
    res.status(500).send({ message: err.message as string });
  }
}
