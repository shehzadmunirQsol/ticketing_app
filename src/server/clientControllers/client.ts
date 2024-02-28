import { clientGetSearchSchema } from '~/schema/client';
import { prisma } from '../prisma';
import { projectCreateSchema, projectGetAllSchema } from '~/schema/project';
import { getUserData } from '~/utils/helper';

/* 
 ---- input ----
 email
 password 
*/
export async function getSearchedClient(req: any, res: any) {
  // const input = req.body;

  try {
    if (!req.query)
      return res.status(400).send({ message: 'payload not found' });
    const input = { ...req.query };
    delete input.routes;

    // Check if the authorization scheme is Bearer and if the token exists

    // const input = JSON.parse(req.body as any);
    const validate = clientGetSearchSchema.safeParse(input);

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

    const options: any = {
      where: {
        is_deleted: false,
        role: 'client',
      },
    };
    console.log({ userData });

    if (validate?.data?.searchQuery) {
      options.where.OR = [];
      options.where.OR.push({
        email: { contains: validate?.data?.searchQuery, mode: 'insensitive' },
      });
      options.where.OR.push({
        username: {
          contains: validate?.data?.searchQuery,
          mode: 'insensitive',
        },
      });
      // options.where.OR.push({
      //   price: { contains: input.searchQuery, mode: 'insensitive' },
      // });
    }

    const projectPromise = prisma.customer.findMany({
      ...options,
    });

    const [projectData] = await Promise.all([projectPromise]);

    return res.status(200).send({ data: projectData });
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
