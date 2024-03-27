import { projectGetSchema } from '~/schema/project';
import { prisma } from '~/server/prisma';
import { getUserData, stringToBoolean } from '~/utils/helper';

/**
 * This function retrieves all tickets based on the user's role and provided query parameters.
 * It filters tickets based on the user's role and additional search criteria.
 * @param req - The request object containing query parameters.
 * @param res - The response object to send back to the client.
 * @returns A response containing ticket data based on the user's role and query parameters.
 */
export async function getProjectAll(req: any, res: any) {
  // const input = req.body;

  try {
    if (!req.query)
      return res.status(400).send({ message: 'payload not found' });
    const input = { ...req.query };
    delete input.routes;

    // Check if the authorization scheme is Bearer and if the token exists

    // const input = JSON.parse(req.body as any);
    const validate = projectGetSchema.safeParse(input);

    if (!validate.success)
      return res.status(400).send({
        message:
          validate?.error && validate?.error?.errors[0]?.message
            ? validate?.error?.errors[0]?.message
            : 'Bad Request',
      });
    // const { jwt, ...data } = validate.data;
    const userData: any = await getUserData(req, res);
    if (!userData) {
      return res.status(400).send({
        message: 'You are not authorized to access!',
      });
    }

    const {
      endDate,
      startDate,
      first,
      rows,
      orderBy,
      is_archive,
      searchQuery,
      completed,
      ...data
    }: any = { ...validate.data };

    const options: any = {
      orderBy: { created_at: orderBy ?? 'desc' },
      skip: first ? +first : 0,
      take: rows ? +rows : 50,
      where: {
        created_by: userData?.id,
        is_deleted: false,
        ...data,
      },
    };
    console.log({ userData });
    if (userData?.role == 'seller_trucker') {
      options.where = {
        OR: [
          {
            ProjectTruckers: {
              some: {
                AND: [
                  {
                    status: { in: ['accepted', 'pending'] },
                  },
                  {
                    trucker_id: userData?.id,
                  },
                ],
              },
            },
          },
          {
            created_by: userData?.id,
          },
        ],
        ProjectStatus: {
          none: {
            created_by: userData?.id,
            is_archive: !stringToBoolean(is_archive),
          },
        },

        is_deleted: false,
        ...data,
      };
    }
    if (userData?.role == 'trucker') {
      options.where = {
        ProjectTruckers: {
          some: {
            AND: [
              {
                status: { in: ['accepted', 'pending'] },
              },
              {
                trucker_id: userData?.id,
              },
            ],
          },
        },
        ProjectStatus: {
          none: {
            created_by: userData?.id,
            is_archive: !stringToBoolean(is_archive),
          },
        },

        is_deleted: false,
        ...data,
      };
    }
    if (userData?.role == 'client') {
      options.where = {
        client_id: userData?.id,
        is_deleted: false,
        ProjectStatus: {
          none: {
            created_by: userData?.id,
            is_archive: !stringToBoolean(is_archive),
          },
        },

        ...data,
      };
    }
    if (userData?.role == 'seller_buyer') {
      options.where = {
        created_by: userData?.id,
        is_deleted: false,
        ProjectStatus: {
          none: {
            created_by: userData?.id,
            is_archive: !stringToBoolean(is_archive),
          },
        },

        ...data,
      };
    }
    if (searchQuery) {
      if (userData?.role == 'seller_trucker') {
        options.where.AND = [];
        options.where.AND.OR = [];
        options.where.AND.OR.push({
          ProjectTruckers: {
            some: {
              AND: [
                {
                  status: { in: ['accepted', 'pending'] },
                },
                {
                  trucker_id: userData?.id,
                },
              ],
            },
          },
        });
        options.where.AND.OR.push({
          created_by: userData?.id,
        });
        options.where.AND.push({
          name: { contains: searchQuery, mode: 'insensitive' },
        });
      } else {
        options.where.OR = [];
        options.where.OR.push({
          name: { contains: searchQuery, mode: 'insensitive' },
        });
      }
    }

    if (startDate && !endDate) {
      const start_date = new Date(startDate)
        ?.toISOString()
        .split('T')[0] as string;
      options.where.start_date = { gte: new Date(start_date) };
    }
    if (endDate && !startDate) {
      const end_Date = new Date(endDate)?.toISOString().split('T')[0] as string;
      options.where.delivery_date = { lte: new Date(end_Date) };
    }
    if (endDate && startDate) {
      const start_date = new Date(startDate)
        ?.toISOString()
        .split('T')[0] as string;
      const end_Date = new Date(endDate)?.toISOString().split('T')[0] as string;
      options.where.start_date = {
        gte: new Date(start_date),
      };
      options.where.delivery_date = {
        lte: new Date(end_Date),
      };
    }
    if (completed) {
      options.where.is_invoiced = stringToBoolean(completed);
    }
    const totalProjectsPromise = prisma.projects.count({
      where: options?.where,
    });

    const projectPromise = prisma.projects.findMany({
      ...options,

      include: {
        ProjectAddress: true,
        Client: true,
      },
    });

    const [projectsTotalData, projectData] = await Promise.all([
      totalProjectsPromise,
      projectPromise,
    ]);

    return res
      .status(200)
      .send({ count: projectsTotalData, data: projectData });
  } catch (err: any) {
    console.log({ msg: err.message });
    res.status(500).send({ message: err.message as string });
  }
}
