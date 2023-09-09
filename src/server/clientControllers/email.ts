
export default async function Emailhandler(req: any, res: any) {
  console.log(req.body,"req.body.email")
  try{
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
          to: [{email: req.body.to}],
          subject: req.body.subject,
          templateId: req.body.template_id,
          params:{...req.body.params}
        })
      };
      fetch('https://api.brevo.com/v3/smtp/email', options)
        .then(response => response.json())
        .then(response => console.log(response))
        .catch(err => console.error(err));

    return res.status(200).send({ message: "email sent succesfully" })

  }catch(e){
    return res.status(500).send({ message: "email Not sent " })


  }
    
 
}