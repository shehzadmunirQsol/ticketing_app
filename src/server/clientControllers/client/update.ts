import { projectCreateSchema, updateProjectTrcuker } from '~/schema/project';
import { prisma } from '~/server/prisma';
import { sendInvitation } from '~/utils/clientMailer';
import { getUserData } from '~/utils/helper';

import { verifyJWT } from '~/utils/jwt';
import { clientEmailLayout } from '~/utils/mailer';
import { createSmartAccount } from '../web3-controller/createAccount';
import { createWeb3Ticket } from '../web3-controller/createWeb3Ticket';

/**
 * This function retrieves all tickets based on the user's role and provided query parameters.
 * It filters tickets based on the user's role and additional search criteria.
 * @param req - The request object containing query parameters.
 * @param res - The response object to send back to the client.
 * @returns A response containing ticket data based on the user's role and query parameters.
 */
export async function updateProjectClient(req: any, res: any) {
  try {
    if (!req.body)
      return res.status(400).send({ message: 'payload not found' });

    const input = req.body;
    const validate = updateProjectTrcuker.safeParse(input);

    if (!validate.success)
      return res.status(400).send({
        message:
          validate?.error && validate?.error?.errors[0]?.message
            ? validate?.error?.errors[0]?.message
            : 'Bad Request',
      });

    const userData: any = await getUserData(req, res);

    const unAuthRole = ['trucker', 'client'];
    const { truckers, ...data } = validate.data;
    const uniqueTrucker = new Set(truckers.map((v: any) => v.trucker_id));
    // check access
    if (!userData || unAuthRole.includes(userData?.role)) {
      return res.status(400).send({
        message: 'You are not authorized to access!',
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
          name: {
            not: {
              in: ['trucker', 'seller_trucker'],
            },
          },
        },
      },
    });
    if (findTruckerRole.length > 0)
      return res.status(400).send({
        message: `User Role Conflict.`,
      });
    //   find project tickets
    const projectTicket = await prisma.projectTickets.findMany({
      where: {
        project_id: data.project_id,
        trucker_id: {
          notIn: truckerArray,
        },
      },
      select: {
        project_id: true,
        trucker_id: true,
        Trucker: {
          select: {
            first_name: true,
            username: true,
          },
        },
      },
    });
    const ticketTruckerArray = projectTicket.map(function (obj) {
      return obj.trucker_id;
    });
    if (projectTicket?.length > 0)
      return res.status(400).send({
        message: `Cant Delete Truckers Tickets Already Created.`,
        data: ticketTruckerArray,
        truckerArray,
      });

    // find if  trucker is already assigned to project or not
    const truckerProject = await prisma.projectTruckers.findMany({
      where: {
        project_id: data?.project_id,
      },
    });

    // Concatenate both arrays
    const truckerUpdatePromise = truckerProject.map(async (value) => {
      if (truckerArray.includes(value?.trucker_id as number)) {
        await prisma.projectTruckers.update({
          where: { id: value.id },
          data: {
            is_deleted: false,
          },
        });
      }
    });
    const truckerCreatePromise = truckerArray.map(async (value) => {
      console.log('i am inside create function');
      const findElement = truckerProject.find(
        (item) => item?.trucker_id === value,
      );
      if (!findElement) {
        console.log('i am inside creating trucker');

        await prisma.projectTruckers.create({
          data: {
            project_id: data?.project_id,
            trucker_id: value,
          },
        });
      }
    });
    const truckerDeletePromise = truckerProject.map(async (value) => {
      if (!truckerArray.includes(value?.trucker_id as number)) {
        console.log(' i am inside delete function');

        await prisma.projectTruckers.updateMany({
          where: {
            project_id: data.project_id,
            trucker_id: { notIn: truckerArray },
          },
          data: {
            is_deleted: true,
          },
        });
      }
    });

    await Promise.all([
      truckerUpdatePromise,
      truckerCreatePromise,
      truckerDeletePromise,
    ]);
    // project trucker Creation
    // const createTrucker = await prisma.projectTruckers.createMany({
    //   data: uniqueValuesArray,
    //   skipDuplicates: true,
    // });

    return res.status(200).send({ truckerInput: truckerProject });
  } catch (err: any) {
    console.log({ err });
    res.status(500).send({ message: err.message as string });
  }
}
