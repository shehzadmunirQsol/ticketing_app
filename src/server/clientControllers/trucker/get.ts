import { projectGetAllSchema } from '~/schema/project';
import { prisma } from '~/server/prisma';
import { getUserData } from '~/utils/helper';

/**
 * This function retrieves all Truckers based on the user's role and provided query parameters.
 
 * @param req - The request object containing query parameters.
 * @param res - The response object to send back to the client.
 * @returns A response containing ticket data based on the user's role and query parameters.
 */
export async function getTruckersAll(req: any, res: any) {
  try {
    // Check if request body is present
    if (!req.query)
      return res.status(400).send({ message: 'payload not found' });

    // Validate the input payload using a schema
    const input = { ...req.query };
    delete input.routes;
    const validate = projectGetAllSchema.safeParse(input);

    // If validation fails, send back an error response
    if (!validate.success)
      return res.status(400).send({
        message:
          validate?.error && validate?.error?.errors[0]?.message
            ? validate?.error?.errors[0]?.message
            : 'Bad Request',
      });
    // Fetch user data from the request and authorization bearer
    const userData: any = await getUserData(req, res);
    if (!userData) {
      return res.status(400).send({
        message: 'You are not authorized to access!',
      });
    }
    const { orderBy, first, rows, ...filterPayload }: any = {
      ...validate.data,
    };
    delete filterPayload.searchQuery;
    delete filterPayload.endDate;
    delete filterPayload.startDate;
    const options: any = {
      orderBy: { created_at: validate.data?.orderBy },
      skip: input.first ? +input.first : 0,
      take: input.rows ? +input.rows : 50,
      where: {
        is_deleted: false,
        ...filterPayload,
        Role: {
          name: {
            equals: 'trucker',
          },
        },
      },
    };
    console.log({ userData });

    if (validate.data.searchQuery) {
      options.where.OR = [];
      options.where.OR.push({
        name: { contains: validate.data.searchQuery, mode: 'insensitive' },
      });
      options.where.OR.push({
        description: {
          contains: validate.data.searchQuery,
          mode: 'insensitive',
        },
      });
      // options.where.OR.push({
      //   price: { contains: input.searchQuery, mode: 'insensitive' },
      // });
    }

    if (validate.data?.startDate) {
      const startDate = new Date(validate.data?.startDate);
      startDate.setDate(startDate.getDate());

      options.where.AND = [];
      options.where.AND.push({ created_at: { gte: startDate } });
    }
    if (validate.data?.endDate) {
      const endDate = new Date(validate.data?.endDate);
      endDate.setDate(endDate.getDate() + 1);

      options.where.AND = options?.AND ?? [];
      options.where.AND.push({ created_at: { lte: endDate } });
    }
    const totalProjectsPromise = prisma.user.count({
      where: options.where,
    });

    const projectPromise = prisma.user.findMany({
      ...options,

      include: {
        Role: true,
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
    res.status(500).send({ message: err.message as string });
  }
}
