import { notificationHandler } from '~/server/lib/notifications.service';
import { sendInvitation } from './clientMailer';
import { clientEmailLayout } from './mailer';

export async function sendNotifications(data: any) {
  const emailData = {
    type: data?.type,
    userData: data?.userData?.first_name ?? 'Owner',
    validate: data?.name,
    subject: `${data?.title} - ${data?.userData?.first_name}`,

    usercontent: `<p style="color: #FFFFFF; font-size: 13px;">${
      data?.userData?.first_name ?? 'Owner'
    } invited you as ${data?.role} in ${data?.name} project.</p>`,
    notfication_content: `${
      data?.userData?.first_name ?? 'Owner'
    } invited you as ${data?.role} in ${data?.name} project.`,
  };
  const clientEmailHTML: string = clientEmailLayout(emailData);

  // send notification to seller
  const notificationPromise = await notificationHandler({
    user_id: data?.notification?.id.toString(),
    document_id: data?.notification?.id.toString(),
    device_id: data?.notification?.device_id,
    type: data?.notification?.type,
    message: emailData?.notfication_content,
    title: emailData?.subject,
    route: `/product-info/`,
  });

  const emailPromise = await sendInvitation({
    email: data?.email,
    from: data?.userData?.first_name ?? 'Owner',
    subject: data?.subject,
    type: data?.type,

    html: clientEmailHTML, // Pass HTML content
  });
  await Promise.all([notificationPromise, emailPromise]);

  return { emailData, emailContent: clientEmailHTML };
}
