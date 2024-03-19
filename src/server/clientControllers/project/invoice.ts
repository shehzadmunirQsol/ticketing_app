import { projectCreateSchema, projectGetDetailSchema } from '~/schema/project';
import { prisma } from '~/server/prisma';
import { sendInvitation } from '~/utils/clientMailer';
import { getUserData } from '~/utils/helper';

import { verifyJWT } from '~/utils/jwt';
import { clientEmailLayout } from '~/utils/mailer';
import { createSmartAccount } from '../web3-controller/createAccount';
import { createWeb3Ticket } from '../web3-controller/createWeb3Ticket';

export async function generateProjectInvoice(req: any, res: any) {
  try {
    if (!req.body)
      return res.status(400).send({ message: 'payload not found' });

    const input = req.body;
    const validate = projectGetDetailSchema.safeParse(input);

    if (!validate.success)
      return res.status(400).send({
        message:
          validate?.error && validate?.error?.errors[0]?.message
            ? validate?.error?.errors[0]?.message
            : 'Bad Request',
      });

    const userData: any = await getUserData(req, res);
    const unAuthRole = ['trucker', 'client'];

    // check access
    if (!userData || unAuthRole.includes(userData?.role as string)) {
      return res.status(400).send({
        message: 'You are not authorized to access!',
      });
    }
    const updateProject = await prisma.projects.updateMany({
      where: {
        id: validate?.data?.id,
        created_by: userData?.id as number,
      },
      data: {
        is_invoiced: true,
      },
    });
    console.log({ project: userData });
    // return res.status(200).send({ project: userData });

    if (updateProject?.count == 0) {
      return res.status(400).send({
        message: 'You are not the owner of this project!',
      });
    }
    return res.status(200).send({ project: updateProject?.count });
  } catch (err: any) {
    console.log({ err });
    res.status(500).send({ message: err.message as string });
  }
}
