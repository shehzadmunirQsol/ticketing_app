const sgMail = require('@sendgrid/mail');
import { NextApiRequest, NextApiResponse } from 'next';
import { clientEmailLayout } from '~/utils/mailer';

sgMail.setApiKey('' + process.env.SENDGRID_API_KEY + '');
export default async function handler(
  req: NextApiRequest,
  res: NextApiResponse,
) {
  try {
    // console.log("REQ.BODY", req.body);
    const sendingEmail = await sgMail.send({
      to: 'shehzadbaloch984@gmail.com', // Your email where you'll receive emails
      // from: req.body.email, // your website email address here
      from: process.env.NEXT_PUBLIC_ADMIN_EMAIL ?? process.env.ADMIN_EMAIL,
      subject: `testinf`,
      html: 'testing',
    });
    console.log({ sendingEmail });
    return res.status(200).json({ message: 'Email sent successfully' });
  } catch (error: any) {
    // console.log(error);
    return res.status(error.statusCode || 500).json({ error });
  }
}
