const sgMail = require('@sendgrid/mail');
import { NextApiRequest, NextApiResponse } from 'next';
import { clientEmailLayout } from '~/utils/mailer';

sgMail.setApiKey('' + process.env.SENDGRID_API_KEY + '');
export default async function handler(
  req: NextApiRequest,
  res: NextApiResponse,
) {
  const html: string = clientEmailLayout(req.body);
  try {
    // console.log("REQ.BODY", req.body);
    const sendingEmail = await sgMail.send({
      to: process.env.NEXT_PUBLIC_ADMIN_EMAIL, // Your email where you'll receive emails
      from: process.env.NEXT_PUBLIC_ADMIN_EMAIL,
      subject: req.body.subject ?? `Xoltan Test`,
      html: req.body.type === 'newsletter' ? `${req.body.email}` : `${html}`,
    });
    console.log({ sendingEmail });
    return res.status(200).json({ message: 'Email sent successfully' });
  } catch (error: any) {
    // console.log(error);
    return res.status(error.statusCode || 500).json({ error });
  }
}
