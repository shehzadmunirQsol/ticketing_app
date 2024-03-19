const sgMail = require('@sendgrid/mail');
import { NextApiRequest, NextApiResponse } from 'next';
import { clientEmailLayout } from '~/utils/mailer';

sgMail.setApiKey(process.env.SENDGRID_API_KEY as string);
export default async function handler(
  req: NextApiRequest,
  res: NextApiResponse,
) {
  console.log(req.body, 'req.body');

  try {
  //  console.log("sent");
    console.log("REQ.BODY", req.body);
    await sgMail.send({
      to: req.body.email, // Your email where you'll receive emails
      from: process.env.NEXT_PUBLIC_ADMIN_EMAIL ?? process.env.ADMIN_EMAIL,
      subject: req.body.subject ?? `Ticketing Test`,
      html: req.body.html,
    });
    console.log('email to', req.body.email);
    return res.status(200).json({ message: 'Email sent successfully' });
  } catch (error: any) {
    // console.log(error);
    return res.status(error.statusCode || 500).json({ error });
  }
}
