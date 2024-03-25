import {
  projectGetAllSchema,
  projectGetSchema,
  projectViewSchema,
} from '~/schema/project';
import { prisma } from '~/server/prisma';
import { getUserData } from '~/utils/helper';

/**
 * This function retrieves all tickets based on the user's role and provided query parameters.
 * It filters tickets based on the user's role and additional search criteria.
 * @param req - The request object containing query parameters.
 * @param res - The response object to send back to the client.
 * @returns A response containing ticket data based on the user's role and query parameters.
 */
export async function getProjectView(req: any, res: any) {
  // const input = req.body;

  try {
    if (!req.query)
      return res.status(400).send({ message: 'payload not found' });
    const input = { ...req.query };
    delete input.routes;

    // Check if the authorization scheme is Bearer and if the token exists

    // const input = JSON.parse(req.body as any);
    const validate = projectViewSchema.safeParse(input);

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

    const { ...data }: any = { ...validate.data };

    const options: any = {
      where: {
        created_by: userData?.id,
        is_deleted: false,
        id: +validate.data?.project_id,
        // ...data,
      },
    };
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

        is_deleted: false,
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

        is_deleted: false,
      };
    }
    if (userData?.role == 'client') {
      options.where = {
        client_id: userData?.id,
        is_deleted: false,
      };
    }
    if (userData?.role == 'seller_buyer') {
      options.where = {
        created_by: userData?.id,
        is_deleted: false,
      };
    }

    const projectPromise = prisma.projects.findFirst({
      ...options,

      include: {
        ProjectAddress: true,
        Client: true,
        ProjectTruckers: {
          include: {
            Trucker: {
              select: {
                id: true,
                first_name: true,
                username: true,
                email: true,
              },
            },
          },
        },
      },
    });

    const [projectData] = await Promise.all([projectPromise]);

    return res.status(200).send({ data: projectData });
  } catch (err: any) {
    console.log({ msg: err.message });
    res.status(500).send({ message: err.message as string });
  }
}
