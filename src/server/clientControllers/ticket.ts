import { prisma } from '../prisma';
import { getUserData } from '~/utils/helper';
import { createSmartAccount } from './web3-controller/createAccount';
import { createWeb3Ticket } from './web3-controller/createWeb3Ticket';
import { verifyJWT } from '~/utils/jwt';
import { ticketCreateSchema } from '~/schema/ticket';
import { projectGetAllSchema } from '~/schema/project';

export async function getAllTicket(req: any, res: any) {
  try {
    if (!req.query)
      return res.status(400).send({ message: 'payload not found' });
    const input = { ...req.query };
    delete input.routes;

    // Check if the authorization scheme is Bearer and if the token exists

    // const input = JSON.parse(req.body as any);
    const validate = projectGetAllSchema.safeParse(input);

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
      start_date,
      first,
      rows,
      orderBy,
      searchQuery,
      ...data
    }: any = { ...validate.data };

    const options: any = {
      orderBy: { created_at: orderBy ?? 'desc' },
      skip: first ? +first : 0,
      take: rows ? +rows : 50,
      where: {
        is_deleted: false,
        ...data,
      },
    };
    console.log('USER1 : ', userData);
    console.log('USER : ', userData?.Role?.name);
    if (userData?.Role?.name == 'trucker') {
      options.where = {
        trucker_id: {
          hasEvery: [userData?.id],
        },
        is_deleted: false,
        ...data,
      };
    }

    if (userData?.Role?.name == 'client') {
      options.where = {
        Projects: {
          client_id: userData?.id,
        },
        is_deleted: false,
        ...data,
      };
    }
    if (userData?.Role?.name == 'seller') {
      options.where = {
        Projects: {
          created_by: userData?.id,
        },
        is_deleted: false,
        ...data,
      };
    }
    if (searchQuery) {
      options.where.OR = [];
      options.where.OR.push({
        status: { contains: searchQuery, mode: 'insensitive' },
      });
      // options.where.OR.push({
      //   project_id: {
      //     contains: searchQuery,
      //     mode: 'insensitive',
      //   },
      // });
    }

    if (start_date) {
      const startDate = new Date(start_date);
      startDate.setDate(startDate.getDate());

      options.where.AND = [];
      options.where.AND.push({ created_at: { gte: startDate } });
    }
    if (endDate) {
      const endDateFormat = new Date(endDate);
      endDateFormat.setDate(endDateFormat.getDate() + 1);

      options.where.AND = options?.AND ?? [];
      options.where.AND.push({ created_at: { lte: endDateFormat } });
    }
    console.log('OPtions : ', options.where);
    const totalTicketProjectsPromise = prisma.projectTickets.count({
      where: options?.where,
    });

    const ticketProjectPromise = prisma.projectTickets.findMany({
      ...options,
    });

    const [projectsTotalData, projectData] = await Promise.all([
      totalTicketProjectsPromise,
      ticketProjectPromise,
    ]);

    return res
      .status(200)
      .send({ count: projectsTotalData, data: projectData });
    return res
      .status(200)
      .send({ count: projectsTotalData, data: projectData });
  } catch (err: any) {
    res.status(500).send({ message: err.message as string });
  }
}

export async function createTicket(req: any, res: any) {
  try {
    if (!req.body)
      return res.status(400).send({ message: 'payload not found' });

    const input = req.body;
    const validate = ticketCreateSchema.safeParse(input);

    if (!validate.success)
      return res.status(400).send({
        message:
          validate?.error && validate?.error?.errors[0]?.message
            ? validate?.error?.errors[0]?.message
            : 'Bad Request',
      });

    const userData: any = await getUserData(req, res);

    const { private_address, ...data } = validate.data;

    const findProject = await prisma.projects.findFirst({
      where: {
        id: data?.project_id,
        is_invoiced: false,
      },
    });

    if (!findProject) {
      return res.status(400).json({
        error: 'Project Id not found',
      });
    }
    // for web3 ticketcreation
    // const decodePrivateAddress: any = await verifyJWT(private_address);
    const private_address1 =
      '1a9060d87234f853cad12f07c98c903ff8138f467a795f9faf0b7cfd48f52510';
    // Load SmartAccount
    const smartAccount = await createSmartAccount({
      private_address: private_address1,
    });

    const txHash = await createWeb3Ticket(smartAccount);
    const payload = {
      trucker_id: userData?.id,
      tx_hash: txHash?.transactionHash,
      ...data,
    };
    console.log('Payload : ', payload);
    const result = await prisma.projectTickets.create({
      data: {
        trucker_id: userData?.id,
        tx_hash: txHash?.transactionHash,
        ...data,
      },
    });
    return res.status(200).send({ project: result });
  } catch (err: any) {
    console.log({ err });
    res.status(500).send({ message: err.message as string });
  }
}
