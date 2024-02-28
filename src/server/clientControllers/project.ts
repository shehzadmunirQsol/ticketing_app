import { signJWT, verifyJWT } from '~/utils/jwt';
import { prisma } from '../prisma';
import { loginCustomerSchema, registerCustomerSchema } from '~/schema/auth';
import bcrypt from 'bcryptjs';
import { projectCreateSchema, projectGetAllSchema } from '~/schema/project';
import { getUserData } from '~/utils/helper';

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
    const { filters } = validate.data;
    const filterPayload: any = { ...filters };
    delete filterPayload.searchQuery;
    delete filterPayload.endDate;
    delete filterPayload.startDate;
    const options: any = {
      orderBy: { created_at: validate.data?.orderBy },
      skip: input.first,
      take: input.rows,
      where: {
        created_by: userData?.id,
        is_deleted: false,
        ...filterPayload,
      },
    };
    console.log({ userData });
    if (userData?.role == 'trucker') {
      options.where = {
        trucker_id: {
          hasEvery: [userData?.id],
        },
        is_deleted: false,
        ...filterPayload,
      };
    }
    if (userData?.role == 'client') {
      options.where = {
        client_id: userData?.id,
        is_deleted: false,
        ...filterPayload,
      };
    }
    if (userData?.role == 'seller') {
      options.where = {
        user_id: userData?.id,
        is_deleted: false,
        ...filterPayload,
      };
    }
    if (filters && filters.searchQuery) {
      options.where.OR = [];
      options.where.OR.push({
        name: { contains: filters.searchQuery, mode: 'insensitive' },
      });
      options.where.OR.push({
        description: { contains: filters.searchQuery, mode: 'insensitive' },
      });
      // options.where.OR.push({
      //   price: { contains: input.searchQuery, mode: 'insensitive' },
      // });
    }

    if (filters && filters?.startDate) {
      const startDate = new Date(filters?.startDate);
      startDate.setDate(startDate.getDate());

      options.where.AND = [];
      options.where.AND.push({ created_at: { gte: startDate } });
    }
    if (filters && filters?.endDate) {
      const endDate = new Date(filters?.endDate);
      endDate.setDate(endDate.getDate() + 1);

      options.where.AND = options?.AND ?? [];
      options.where.AND.push({ created_at: { lte: endDate } });
    }
    const totalProjectsPromise = prisma.projects.count({
      where: options.where,
    });

    const projectPromise = prisma.projects.findMany({
      ...options,
      include: {
        ProjectAddress: true,
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
  // const input = req.body;

  try {
    if (!req.body)
      return res.status(400).send({ message: 'payload not found' });

    const input = JSON.parse(req.body);
    const validate = projectCreateSchema.safeParse(input);

    if (!validate.success)
      return res.status(400).send({
        message:
          validate?.error && validate?.error?.errors[0]?.message
            ? validate?.error?.errors[0]?.message
            : 'Bad Request',
      });
    const { ...data } = validate.data;
    const userData: any = await getUserData(req, res);
    if (!userData) {
      return res.status(400).send({
        message: 'You are not authorized to access!',
      });
    }
    const unAuthRole = ['trucker', 'client'];
    if (unAuthRole.includes(userData?.role)) {
      return res.status(400).send({ message: "You're not authorized" });
    }
    // Create a new User instance with the hashed password
    const result = await prisma.projects.create({
      data: {
        created_by: userData?.id,
        user_id: userData?.id,

        ...data,
      },
    });

    return res.status(200).send({ project: result });
  } catch (err: any) {
    console.log({ err });
    res.status(500).send({ message: err.message as string });
  }
}
