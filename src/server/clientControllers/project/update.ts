import { projectCreateSchema } from '~/schema/project';
import { prisma } from '~/server/prisma';
import { sendInvitation } from '~/utils/clientMailer';
import { getUserData } from '~/utils/helper';

import { verifyJWT } from '~/utils/jwt';
import { clientEmailLayout } from '~/utils/mailer';
import { createSmartAccount } from '../web3-controller/createAccount';
import { createWeb3Ticket } from '../web3-controller/createWeb3Ticket';

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

    const { truckers, start_date, delivery_date, address, client, ...data } =
      validate.data;
    // for web3 project creation

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
