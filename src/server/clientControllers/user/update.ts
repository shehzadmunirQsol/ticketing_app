import { updateCustomerSchema } from '~/schema/auth';
import { prisma } from '~/server/prisma';
import { getUserData } from '~/utils/helper';

/**
 * This function handles the update of a ticket.
 * It updates the specified ticket with new data provided in the request payload.
 * @param req - The request object containing the payload.
 * @param res - The response object to send back to the client.
 * @returns A response indicating success or failure of the ticket update process.
 */
export async function updateUser(req: any, res: any) {
  try {
    // Check if request body is present
    if (!req.body)
      return res.status(400).send({ message: 'payload not found' });

    // Validate the input payload using a schema
    const input = req.body;
    const validate: any = updateCustomerSchema.safeParse(input);

    // If validation fails, send back an error response
    if (!validate.success)
      return res.status(400).send({
        message:
          validate?.error && validate?.error?.errors[0]?.message
            ? validate?.error?.errors[0]?.message
            : 'Bad Request',
      });

    // Extract update values from validated data

    // Fetch user data from the request
    const userData: any = await getUserData(req, res);

    // Update the ticket in the database
    const customerUpdate = await prisma.user.update({
      where: {
        id: userData?.id,
      },
      data: { ...validate?.data },
    });

    // If no tickets are updated, send back an error response
    if (!customerUpdate) {
      return res.status(400).json({
        error: 'Record Not Found',
      });
    }

    // Send back a success response with the updated ticket data
    return res.status(200).send({ data: customerUpdate, success: true });
  } catch (error: any) {
    // Handle any errors that occur during the ticket update process
    res.status(500).send({ message: error?.message as string, success: false });
  }
}
