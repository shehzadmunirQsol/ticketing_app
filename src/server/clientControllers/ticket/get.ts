import { projectGetAllSchema } from '~/schema/project';
import { prisma } from '~/server/prisma';
import { getUserData } from '~/utils/helper';

/**
 * This function retrieves all tickets based on the user's role and provided query parameters.
 * It filters tickets based on the user's role and additional search criteria.
 * @param req - The request object containing query parameters.
 * @param res - The response object to send back to the client.
 * @returns A response containing ticket data based on the user's role and query parameters.
 */
export async function getAllTicket(req: any, res: any) {
  try {
    // Check if query parameters are present
    if (!req.query)
      return res.status(400).send({ message: 'payload not found' });

    // Extract query parameters and validate them using a schema
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

    // Fetch user data from the request
    const userData: any = await getUserData(req, res);

    // If user data is not present, send back an error response
    if (!userData) {
      return res.status(400).send({
        message: 'You are not authorized to access!',
      });
    }

    // Extract pagination and search parameters from validated data
    const {
      endDate,
      start_date,
      first,
      rows,
      orderBy,
      searchQuery,
      ...data
    }: any = { ...validate.data };

    // Define options for querying tickets based on user's role and search criteria
    const options: any = {
      orderBy: { created_at: orderBy ?? 'desc' },
      skip: first ? +first : 0,
      take: rows ? +rows : 50,
      where: {
        is_deleted: false,
        ...data,
      },
    };

    // Apply role-specific filters
    if (userData?.role == 'trucker') {
      options.where = {
        trucker_id: userData?.id,
        is_deleted: false,
        ...data,
      };
    } else if (userData?.role == 'seller_trucker') {
      options.where = {
        OR: [
          { trucker_id: userData?.id },
          {
            Projects: {
              created_by: userData?.id,
            },
          },
        ],
        is_deleted: false,
        ...data,
      };
    } else if (userData?.role == 'client') {
      options.where = {
        Projects: {
          client_id: userData?.id,
        },
        is_deleted: false,
        ...data,
      };
    } else if (userData?.role == 'seller_buyer') {
      options.where = {
        Projects: {
          created_by: userData?.id,
        },
        is_deleted: false,
        ...data,
      };
    }

    // Apply search query filter
    if (searchQuery) {
      options.where.OR = [];
      options.where.OR.push({
        status: { contains: searchQuery, mode: 'insensitive' },
      });
    }

    // Apply start date filter
    if (start_date) {
      const startDate = new Date(start_date);
      startDate.setDate(startDate.getDate());

      options.where.AND = [];
      options.where.AND.push({ created_at: { gte: startDate } });
    }

    // Apply end date filter
    if (endDate) {
      const endDateFormat = new Date(endDate);
      endDateFormat.setDate(endDateFormat.getDate() + 1);

      options.where.AND = options?.AND ?? [];
      options.where.AND.push({ created_at: { lte: endDateFormat } });
    }

    // Query total number of ticket projects and ticket project data
    const totalTicketProjectsPromise = prisma.projectTickets.count({
      where: options?.where,
    });

    const ticketProjectPromise = prisma.projectTickets.findMany({
      ...options,
    });

    // Await both promises and send back a response with the ticket data
    const [projectsTotalData, projectData] = await Promise.all([
      totalTicketProjectsPromise,
      ticketProjectPromise,
    ]);

    return res
      .status(200)
      .send({ count: projectsTotalData, data: projectData });
  } catch (err: any) {
    // Handle any errors that occur during the retrieval process
    res.status(500).send({ message: err.message as string });
  }
}
