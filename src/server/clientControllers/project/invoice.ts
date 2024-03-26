import { projectGetDetailSchema } from '~/schema/project';
import { prisma } from '~/server/prisma';
import { getUserData } from '~/utils/helper';
import { getHtmlContent } from '../pdf';

/**
 * Generates an invoice for a project based on the provided request body.
 * Updates the project status to 'invoiced' if not already done.
 * Generates HTML content for the invoice.
 * Sends the HTML invoice along with the project update count as response.
 *
 * @param  req - The request object containing the payload.
 * @param  res - The response object to send back the generated invoice.
 * @returns - Response containing the HTML invoice and project update count.
 */
export async function generateProjectInvoice(req: any, res: any) {
  try {
    // Check if request body is present
    if (!req.body)
      return res.status(400).send({ message: 'payload not found' });

    // Validate the input payload using a schema
    const input = req.body;
    const validate = projectGetDetailSchema.safeParse(input);

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
    const unAuthRole = ['trucker', 'client'];

    // If user is not authenticated or belongs to unauthorized role, send back an error response
    if (!userData || unAuthRole.includes(userData?.role as string)) {
      return res.status(400).send({
        message: 'You are not authorized to access!',
      });
    }

    // Retrieve project details from the database
    const projectData = await prisma.projects.findFirst({
      orderBy: { created_at: 'asc' },
      where: { id: validate?.data?.id },
      include: {
        Client: {
          select: {
            first_name: true,
            username: true,
            email: true,
            phone_number: true,
          },
        },
        User: {
          select: {
            first_name: true,
            username: true,
            email: true,
            phone_number: true,
          },
        },
        ProjectTickets: {
          select: {
            trucker_id: true,
            tx_hash: true,
            Trucker: true,
            status: true,
          },
        },
        ProjectAddress: true,
      },
    });

    // Check if the user is the owner of the project
    if (projectData?.created_by === userData?.id) {
      let updateProject;

      // If the project is not already invoiced, update its status to 'invoiced'
      if (!projectData?.is_invoiced) {
        updateProject = await prisma.projects.updateMany({
          where: {
            id: validate?.data?.id,
            created_by: userData?.id as number,
          },
          data: {
            is_invoiced: true,
          },
        });
      }

      // Generate HTML content for the invoice
      const htmlInvoice = await getHtmlContent(projectData);

      // Send the HTML invoice along with the project update count as response
      return res
        .status(200)
        .send({ htmlInvoice, project: updateProject?.count ?? 0 });
    } else {
      // If the user is not the owner of the project, send back an error response
      return res.status(400).send({
        message: 'You are not the owner of this project!',
      });
    }
  } catch (err: any) {
    // Handle any errors that occur during the process
    res.status(500).send({ message: err.message as string });
  }
}
