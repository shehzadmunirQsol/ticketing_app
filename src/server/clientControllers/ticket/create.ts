import { prisma } from '~/server/prisma';
import { createSmartAccount } from '../web3-controller/createAccount';
import { createWeb3Ticket } from '../web3-controller/createWeb3Ticket';
import { ticketCreateSchema } from '~/schema/ticket';
import { getUserData } from '~/utils/helper';
import { verifyJWT } from '~/utils/jwt';
import {
  notificationHandler,
  notificationsMessages,
  notificationsTypes,
} from '~/server/lib/notifications.service';

/**
 * This function handles the creation of a ticket.
 * It performs several validations and checks before creating the ticket.
 * @param req - The request object containing the payload.
 * @param res - The response object to send back to the client.
 * @returns A response indicating success or failure of the ticket creation process.
 */
export async function createTicket(req: any, res: any) {
  try {
    // Check if request body is present
    if (!req.body)
      return res.status(400).send({ message: 'payload not found' });

    // Validate the input payload using a schema
    const input = req.body;
    const validate = ticketCreateSchema.safeParse(input);

    // If validation fails, send back an error response
    if (!validate.success)
      return res.status(400).send({
        message:
          validate?.error && validate?.error?.errors[0]?.message
            ? validate?.error?.errors[0]?.message
            : 'Bad Request',
      });

    // Fetch user data from the request
    const userData: any = await getUserData(req, res);

    // Define unauthorized roles
    const unAuthRole = ['seller_buyer', 'client'];

    // If user is not authenticated or belongs to unauthorized role, send back an error response
    if (!userData || unAuthRole.includes(userData?.role)) {
      return res.status(400).send({
        message: 'You are not authorized to access!',
      });
    }
    console.log({ userData });
    // Extract private address from input data
    const { private_address, ...data } = validate.data;

    // Find the project associated with the provided project ID
    const findProject = await prisma.projects.findFirst({
      where: {
        id: data?.project_id,
        is_invoiced: false,
        ProjectTruckers: {
          some: { AND: [{ trucker_id: userData?.id }, { status: 'accepted' }] },
        },
      },
      include: {
        User: {
          select: {
            id: true,
          },
        },
      },
    });

    // If project is not found, send back an error response
    if (!findProject) {
      return res.status(400).json({
        error: 'Project data not found',
      });
    }

    // Decode private address using JWT
    const decodePrivateAddress: any = await verifyJWT(private_address);

    // Create a smart account using the decoded private address
    const smartAccount = await createSmartAccount({
      private_address: decodePrivateAddress?.address,
    });

    // Create a ticket on the blockchain using the smart account
    const txHash = await createWeb3Ticket(smartAccount);

    // Create the ticket in the database
    const result = await prisma.projectTickets.create({
      data: {
        trucker_id: userData?.id,
        tx_hash: txHash?.transactionHash,
        ...data,
      },
    });

    // send Notification to  trucker
    const truckerNotification = notificationHandler({
      user_id: userData?.id.toString(),
      document_id: userData?.id.toString(),
      type: notificationsTypes.SUCCESS,
      message: notificationsMessages?.ticketCreated({
        project: findProject,
        ...result,
        userData,
      }),
      route: `/product-info/${txHash?.transactionHash}`,
    });
    // send notification to seller
    const sellerNotification = notificationHandler({
      user_id: findProject?.User?.id.toString(),
      document_id: findProject?.User?.id.toString(),
      type: notificationsTypes.SUCCESS,
      message: notificationsMessages?.projectTicketCreated({
        project: findProject,
        ...result,
        userData,
      }),
      route: `/product-info/${txHash?.transactionHash}`,
    });
    const notifications = [truckerNotification, sellerNotification];
    const dataResult = await Promise.all(notifications);
    // Send back a success response with the created ticket
    return res.status(200).send({ ticket: result });
  } catch (err: any) {
    // Handle any errors that occur during the ticket creation process
    console.log({ err });
    res.status(500).send({ message: err.message as string });
  }
}
