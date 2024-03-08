import { prisma } from '../prisma';
import {
  inviteTruckerSchema,
  projectCreateSchema,
  projectGetAllSchema,
} from '~/schema/project';
import { sendInvitation } from '~/utils/clientMailer';
import { getUserData } from '~/utils/helper';

/* 
 ---- input ----
 email
 password 
*/
export async function getTruckersAll(req: any, res: any) {
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
    const filterPayload: any = { ...validate.data };
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
export async function inviteUser(req: any, res: any) {
  // const input = req.body;

  try {
    if (!req.body)
      return res.status(400).send({ message: 'payload not found' });

    const input = req.body;
    const validate = inviteTruckerSchema.safeParse(input);

    if (!validate.success)
      return res.status(400).send({
        message:
          validate?.error && validate?.error?.errors[0]?.message
            ? validate?.error?.errors[0]?.message
            : 'Bad Request',
      });
    // const existingUser = await User.findOne({ email });

    const userData: any = await getUserData(req, res);
    if (!userData) {
      return res.status(400).send({
        message: 'You are not authorized to access!',
      });
    }
    const findRole = await prisma.role.findFirst({
      where: {
        id: userData.role_id,
      },
    });
    const unAuthRole = ['trucker', 'client'];
    if (findRole && unAuthRole.includes(findRole?.name)) {
      return res.status(400).send({ message: "You're not authorized" });
    }
    const existingUser = await prisma.user.findFirst({
      where: { email: validate.data?.email },
    });

    if (existingUser) {
      return res.status(400).json({
        error: 'Email already exists. Please use a different email.',
      });
    }
    const { type, ...data } = validate.data;
    const role = await prisma.role.findFirst({
      where: { name: type },
    });
    const truckerData = await prisma.user.upsert({
      where: {
        email: data.email,
      },
      update: {},
      create: {
        ...data,
        role_id: role?.id,
      },
    });
    if (truckerData)
      await sendInvitation({
        email: truckerData?.email,
        from: userData?.first_name ?? 'Owner',
        subject: `Platform Invitation`,
        type: 'project-invitation',
        raw: `<p> ${
          userData?.first_name ?? 'Seller/Buyer'
        } invited you as client in ticketing platform. </p>`,
      });
    // Create a new User instance with the hashed password

    return res.status(200).send({ trucker: truckerData, success: true });
  } catch (err: any) {
    console.log({ err });
    res.status(500).send({ message: err.message as string });
  }
}
