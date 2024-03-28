import { updateProjectClientSchema } from '~/schema/project';
import { notificationsTypes } from '~/server/lib/notifications.service';
import { prisma } from '~/server/prisma';
import { getUserData } from '~/utils/helper';
import { sendNotifications } from '~/utils/utility';

/**
 * This function retrieves all tickets based on the user's role and provided query parameters.
 * It filters tickets based on the user's role and additional search criteria.
 * @param req - The request object containing query parameters.
 * @param res - The response object to send back to the client.
 * @returns A response containing ticket data based on the user's role and query parameters.
 */

export async function updateProjectClient(req: any, res: any) {
  try {
    // check if payload is present
    if (!req.body)
      return res.status(400).send({ message: 'payload not found' });

    const input = req.body;

    const validate = updateProjectClientSchema.safeParse(input);
    // validation if data present is accurate
    if (!validate.success)
      return res.status(400).send({
        message:
          validate?.error && validate?.error?.errors[0]?.message
            ? validate?.error?.errors[0]?.message
            : 'Bad Request',
      });
    // get user dat from Authorization token
    const userData: any = await getUserData(req, res);

    const unAuthRole = ['trucker', 'client'];
    const { project_id, address, client, ...data } = validate.data;
    // check access of user
    if (!userData || unAuthRole.includes(userData?.role)) {
      return res.status(400).send({
        message: 'You are not authorized to access!',
      });
    }

    //   find project tickets
    const projectInfo = await prisma.projects.findFirst({
      where: {
        id: project_id,
        is_invoiced: false,
        is_deleted: false,
        created_by: userData?.id,
      },
      include: {
        ProjectTickets: true,
      },
    });

    if (!projectInfo)
      return res.status(400).send({
        message: `You're not authorize to access this.`,
      });
    if (projectInfo?.ProjectTickets?.length > 0 && client)
      return res.status(400).send({
        message: `You Cant update client information`,
      });
    let clientData;
    if (client) {
      clientData = await prisma.user.upsert({
        where: {
          email: client.email,
        },
        update: {},
        create: {
          ...client,
          role_id: 5,
        },
      });
    }
    // find if  trucker is already assigned to project or not
    const clientInfo = await prisma.projects.update({
      where: {
        id: project_id,
      },
      data: {
        client_id: clientData?.id ?? projectInfo?.client_id,
        ...data,
      },
    });
    if (clientData) {
      await sendNotifications({
        userData,
        name: projectInfo?.name,
        email: clientData.email,
        type: 'project-invitation',
        title: 'Project Invitation',
        role: 'client',
        notification: {
          id: clientData?.id.toString(),
          device_id: clientData?.device_id,
          type: notificationsTypes.SUCCESS,
          route: `/product-info/`,
        },
      });
    }
    await Promise.all([clientInfo]);

    return res.status(200).send({ truckerInput: clientInfo });
  } catch (err: any) {
    console.log({ err });
    res.status(500).send({ message: err.message as string });
  }
}
