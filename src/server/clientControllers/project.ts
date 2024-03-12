import { prisma } from '../prisma';
import { projectCreateSchema, projectGetAllSchema } from '~/schema/project';
import { sendInvitation } from '~/utils/clientMailer';
import { getUserData } from '~/utils/helper';
import { createSmartAccount } from './web3-controller/createAccount';
import { createWeb3Project } from './web3-controller/createWeb3Project';
import { verifyJWT } from '~/utils/jwt';
import { clientEmailLayout } from '~/utils/mailer';

/* 
 ---- input ----
 email
 password 
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
    const userPromise = await prisma.user.findFirst({
      where: {
        id: userData?.id ?? 0,
      },
      include: {
        Role: true,
      },
    });
    if (!userPromise)
      return res.status(400).send({
        message: 'You are not authorized to access!',
      });
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
        created_by: userData?.id,
        is_deleted: false,
        ...data,
      },
    };
    console.log({ userData });
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
        client_id: userData?.id,
        is_deleted: false,
        ...data,
      };
    }
    if (userData?.Role?.name == 'seller') {
      options.where = {
        created_by: userData?.id,
        is_deleted: false,
        ...data,
      };
    }
    if (searchQuery) {
      options.where.OR = [];
      options.where.OR.push({
        name: { contains: searchQuery, mode: 'insensitive' },
      });
      options.where.OR.push({
        description: {
          contains: searchQuery,
          mode: 'insensitive',
        },
      });
      // options.where.OR.push({
      //   price: { contains: input.searchQuery, mode: 'insensitive' },
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
    res.status(500).send({ message: err.message as string });
  }
}
export async function createProject(req: any, res: any) {
  try {
    if (!req.body)
      return res.status(400).send({ message: 'payload not found' });

    const input = req.body;
    const validate = projectCreateSchema.safeParse(input);

    if (!validate.success)
      return res.status(400).send({
        message:
          validate?.error && validate?.error?.errors[0]?.message
            ? validate?.error?.errors[0]?.message
            : 'Bad Request',
      });

    const userData: any = await getUserData(req, res);

    const unAuthRole = ['trucker', 'client'];

    const {
      truckers,
      start_date,
      delivery_date,
      address,
      private_address,
      client,
      ...data
    } = validate.data;
    // for web3 project creation
    const decodePrivateAddress: any = await verifyJWT(private_address);

    const smartAccount = await createSmartAccount({
      private_address: decodePrivateAddress.address,
    });
    const smartAccountAddress = await smartAccount.getAccountAddress();
    await createWeb3Project(smartAccount, 'abcd', data.total_rounds);

    // check access
    if (!userData || unAuthRole.includes(userData?.role)) {
      return res.status(400).send({
        message: 'You are not authorized to access!',
      });
    }
    //  address length should be less than or equal to 2
    if (address?.length > 2)
      return res.status(400).send({
        message: `The assign truckers length should be less than or equal to  ${data?.total_rounds}.`,
      });
    // The assign truckers length should be less than or equal to number of rounds
    if (truckers.length > data?.total_rounds)
      // check if user is authorizer
      return res.status(400).send({
        message: `The assign truckers length should be less than or equal to  ${data?.total_rounds}.`,
      });
    const uniqueAddress = new Set(address.map((v: any) => v.address_type));
    const uniqueTrucker = new Set(truckers.map((v: any) => v.trucker_id));
    // check duplicate address
    if (uniqueAddress.size < address.length) {
      return res.status(400).send({
        message: `Duplicate Address Found.`,
      });
    }
    // check duplicate truckers
    if (uniqueTrucker.size < truckers.length) {
      return res.status(400).send({
        message: `Duplicate Trucker Found.`,
      });
    }
    // check if truckers have trucker role
    const truckerArray = truckers.map(function (obj) {
      return obj.trucker_id;
    });
    const findTruckerRole = await prisma.user.findMany({
      where: {
        id: { in: truckerArray },
        Role: {
          name: { not: 'trucker' },
        },
      },
    });
    if (findTruckerRole.length > 0)
      return res.status(400).send({
        message: `User Role Conflict.`,
        findTruckerRole,
      });
    const clientData = await prisma.user.upsert({
      where: {
        email: client.email,
      },
      update: {},
      create: {
        ...client,
        role_id: 5,
      },
    });
    // Create a new User instance with the hashed password
    const result = await prisma.projects.create({
      data: {
        created_by: userData?.id,
        start_date: new Date(start_date),
        delivery_date: new Date(delivery_date),

        ...data,
        client_id: clientData?.id,
        ProjectAddress: {
          createMany: {
            data: address,
          },
        },
        ProjectTruckers: {
          createMany: {
            data: truckers,
          },
        },
      },
    });
    const emaildata = {
      type: 'project-invitation',
      userData: userData?.first_name ?? 'Owner',
      validate: validate?.data?.name,
    };
    const clientEmailHTML: string = clientEmailLayout(emaildata);
    if (clientData)
      await sendInvitation({
        email: clientData?.email,
        from: userData?.first_name ?? 'Owner',
        subject: `Project Invitation - ${validate?.data?.name}`,
        type: 'project-invitation',
        // raw: `<p> ${userData?.first_name ?? 'Owner'} invited you as client in ${
        //   validate?.data?.name
        // } project. </p>`,
        html: clientEmailHTML, // Pass HTML content
      });

    return res.status(200).send({ project: result });
  } catch (err: any) {
    console.log({ err });
    res.status(500).send({ message: err.message as string });
  }
}
