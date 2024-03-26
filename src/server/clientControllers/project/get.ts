import { projectGetAllSchema, projectGetSchema } from '~/schema/project';
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
      options.where.OR = [];
      options.where.OR.push({
        name: { contains: searchQuery, mode: 'insensitive' },
      });
      if (userData?.role == 'seller_trucker') {
        options.where.AND = [];
        options.where.AND.push({
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
        options.where.AND.push({
          created_by: userData?.id,
        });
      }

      // options.where.OR.push({
      //   price: { contains: input.searchQuery, mode: 'insensitive' },
      // });
    }

    if (startDate) {
      const start_date = new Date(startDate);
      start_date.setDate(start_date.getDate());

      options.where.AND = [];
      options.where.AND.push({ created_at: { gte: start_date } });
    }
    if (endDate) {
      const endDateFormat = new Date(endDate);
      endDateFormat.setDate(endDateFormat.getDate() + 1);

      options.where.AND = options?.AND ?? [];
      options.where.AND.push({ created_at: { lte: endDateFormat } });
    }
    // if (is_archive) {
    //   options.where = {
    //     ...options.where,
    //     ProjectStatus: {
    //       some: {
    //         AND: [{ created_by: userData?.id }, { is_archive }],
    //       },
    //     },
    //   };
    // }
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
