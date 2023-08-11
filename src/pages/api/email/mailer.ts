"use strict";
import { NextApiRequest, NextApiResponse } from "next";
import nodemailer from "nodemailer";

export default async function handler(req: NextApiRequest, res: NextApiResponse) {
    // console.log(req.body, "inside mailer")
    // const transporter = nodemailer.createTransport({
    //     host: "smtp-relay.brevo.com",
    //     port: 465,
    //     secure: true,
    //     auth: {
    //         user: "marketing@winnar.com",
    //         pass: "NC5KIMWYXEkVwpcy",
    //     },
    // });

    // const mailOptions = {
    //     from: '"Winnar " <no-reply@winnar.com>',
    //     to: req.body?.email,
    //     subject: `Winnar`,
    //     html: `<html>
    //             <body>
    //                 <p>html email</p>
    //             </body> 
    //           </html>`,

    // };

    // const info = await transporter.sendMail(mailOptions, (error: any, info: any) => {
    //     if (error) {
    //         return console.log(error)
    //     }
    //     console.log('Message sent: %s', info.messageId)
    // });


    const options:any = {
        method: 'POST',
        headers: {
          accept: 'application/json',
          'content-type': 'application/json',
          'api-key': process.env.BREVO_EMAIL_API_KEY
        },
        body: JSON.stringify({
          sender: {name: 'Winnar', email: 'no-reply@winnar.com'},
        //   params: {FNAME: 'HASSAN', LNAME: 'SHAN'},
          to: [{email: 'hassanshan675@gmail.com'}],
          subject: 'winnar test',
          templateId: 1
        })
      };
      
      fetch('https://api.brevo.com/v3/smtp/email', options)
        .then(response => response.json())
        .then(response => console.log(response))
        .catch(err => console.error(err));

    return res.status(200).send({ message: "no message" })
}



