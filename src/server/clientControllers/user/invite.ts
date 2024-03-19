import { inviteTruckerSchema, projectGetAllSchema } from '~/schema/project';
import { prisma } from '~/server/prisma';
import { sendInvitation } from '~/utils/clientMailer';
import { getUserData } from '~/utils/helper';
import { clientEmailLayout } from '~/utils/mailer';

export async function inviteUser(req: any, res: any) {
  // const input = req.body;

  try {
    if (!req.body)
      return res.status(400).send({ message: 'payload not found' });

    const input = req.body;
    const validate = inviteTruckerSchema.safeParse(input);

    if (!validate.success)
      return res.status(400).send({
        message:
          validate?.error && validate?.error?.errors[0]?.message
            ? validate?.error?.errors[0]?.message
            : 'Bad Request',
      });
    // const existingUser = await User.findOne({ email });

    const userData: any = await getUserData(req, res);
    if (!userData) {
      return res.status(400).send({
        message: 'You are not authorized to access!',
      });
    }
    const findRole = await prisma.role.findFirst({
      where: {
        id: userData.role_id,
      },
    });
    const unAuthRole = ['trucker', 'client'];
    if (findRole && unAuthRole.includes(findRole?.name)) {
      return res.status(400).send({ message: "You're not authorized" });
    }
    const existingUser = await prisma.user.findFirst({
      where: { email: validate.data?.email },
    });

    if (existingUser) {
      return res.status(400).json({
        error: 'Email already exists. Please use a different email.',
      });
    }
    const { type, ...data } = validate.data;
    const role = await prisma.role.findFirst({
      where: { name: type },
    });
    const truckerData = await prisma.user.upsert({
      where: {
        email: data.email,
      },
      update: {},
      create: {
        ...data,
        role_id: role?.id,
      },
    });
    const emaildata = {
      type: 'project-invitation',
      userData: userData?.first_name ?? 'Owner',
    };
    const clientEmailHTML: string = clientEmailLayout(emaildata);
    if (truckerData)
      await sendInvitation({
        email: truckerData?.email,
        from: userData?.first_name ?? 'Owner',
        subject: `Platform Invitation`,
        type: 'platfrom-invitation',
        html: clientEmailHTML,
      });
    // Create a new User instance with the hashed password

    return res.status(200).send({ trucker: truckerData, success: true });
  } catch (err: any) {
    console.log({ err });
    res.status(500).send({ message: err.message as string });
  }
}
