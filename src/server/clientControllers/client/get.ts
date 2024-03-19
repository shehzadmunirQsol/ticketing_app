import { clientGetSearchSchema } from '~/schema/client';
import { projectCreateSchema, projectGetAllSchema } from '~/schema/project';
import { prisma } from '~/server/prisma';
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
        Role: {
          name: {
            in: 'client',
          },
        },
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
      options.where.OR.push({
        first_name: {
          contains: validate?.data?.searchQuery,
          mode: 'insensitive',
        },
      });
    }

    const projectPromise = prisma.user.findMany({
      ...options,
      select: {
        id: true,
        email: true,
        first_name: true,
        username: true,
        phone_number: true,
        role_id: true,
      },
    });

    const [projectData] = await Promise.all([projectPromise]);

    return res.status(200).send({ data: projectData });
  } catch (err: any) {
    res.status(500).send({ message: err.message as string });
  }
}
