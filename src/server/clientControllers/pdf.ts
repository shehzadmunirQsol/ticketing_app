// import { promisify } from 'util';
// import pdf from 'html-pdf';
// import fs from 'fs';
const html_to_pdf = require('html-pdf-node');
/* 
 ---- input ----
 email
 password 
*/
// const pdfAsync = promisify(pdf.create);
// const readFileAsync = promisify(fs.readFile);
async function getHtmlContent(data: any) {
  const htmlContent = ` 
  <!DOCTYPE html>
<html lang="en">
   <head>
      <meta charset="utf-8" />
      <meta name="viewport" content="width=device-width, initial-scale=1" />
      <title>Invoice</title>
      <!-- Favicon -->
      <link rel="icon" href="./images/favicon.png" type="image/x-icon" />
      <!-- Invoice styling -->
      <style>
         body {
         font-family: 'Helvetica Neue', 'Helvetica', Helvetica, Arial, sans-serif;
         text-align: center;
         color: #777;
         }
         body h1 {
         font-weight: 300;
         margin-bottom: 0px;
         padding-bottom: 0px;
         color: #000;
         }
         body h3 {
         font-weight: 300;
         margin-top: 10px;
         margin-bottom: 20px;
         font-style: italic;
         color: #555;
         }
         body a {
         color: #06f;
         }
         .invoice-box {
         max-width: 800px !important;
         margin: auto;
         padding: 30px;
         border: 1px solid #eee;
         box-shadow: 0 0 10px rgba(0, 0, 0, 0.15);
         font-size: 16px;
         line-height: 24px;
         font-family: 'Helvetica Neue', 'Helvetica', Helvetica, Arial, sans-serif;
         color: #555;
         }
         .invoice-box table {
         width: 100%;
         line-height: inherit;
         text-align: left;
         border-collapse: collapse;
         }
         .invoice-box .title{
         width:120px;
         height:120px
         }
         .invoice-box table td {
         padding: 5px;
         vertical-align: top;
         }
         .invoice-box table .top tr td:nth-child(2) {
         text-align: right;
         }
         .invoice-box table .information tr td:nth-child(2) {
         text-align: right;
         }
         .invoice-box table tr.top table td {
         padding-bottom: 20px;
         }
         .invoice-box table tr.top table td.title {
         font-size: 45px;
         line-height: 45px;
         color: #333;
         }
         .invoice-box table tr.information table td {
         padding-bottom: 40px;
         }
         .invoice-box table tr.heading td {
         background: #eee;
         border-bottom: 1px solid #ddd;
         font-weight: bold;
         }
         .invoice-box table tr.heading td:nth-child(4) {
         text-align:right;
         }
         .invoice-box table tr.item td {
         border-bottom: 1px solid #eee;
         }
         .invoice-box table tr.item td:nth-child(4) {
         border-bottom: 1px solid #eee;
         text-align:right;
         }
         .invoice-box table tr.item.last td {
         border-bottom: none; 
         }
         .invoice-box table tr.total td:nth-child(4) {
         border-top: 2px solid #eee;
         font-weight: bold;
         text-align:right;
         }
         @media only screen and (max-width: 600px) {
         .invoice-box table tr.top table td {
         width: 100%;
         display: block;
         text-align: center;
         }
         .invoice-box table tr.information table td {
         width: 100%;
         display: block;
         text-align: center;
         }
         }
      </style>
   </head>
   <body>
      <br />
      <div class="invoice-box">
         <table>
            <tr class="top">
               <td colspan="4">
                  <table>
                     <tr>
                        <td class="title">
                           <img src="https://img.freepik.com/free-vector/bird-colorful-logo-gradient-vector_343694-1365.jpg?size=338&ext=jpg&ga=GA1.1.735520172.1710720000&semt=sph" alt="Company logo" style="width: 100%; max-width: 300px" />
                        </td>
                        <td>
                           Invoice #: 123<br />
                           Created: January 1, 2023
                        </td>
                     </tr>
                  </table>
               </td>
            </tr>
            <tr class="information">
               <td colspan="4">
                  <table>
                     <tr>
                        <td>
                           From: Sparksuite, Inc.<br />
                           12345 Sunny Road<br />
                           Sunnyville, TX 12345
                        </td>
                        <td>
                           To: Acme Corp.<br />
                           John Doe<br />
                           john@example.com
                        </td>
                     </tr>
                  </table>
               </td>
            </tr>
            <tr class="heading">
               <td>Ticket</td>
               <td>Tx Hash</td>
               <td>Trucker</td>
               <td>Status</td>
            </tr>
            <tr class="item">
               <td>Website design</td>
               <td><a href="https://mumbai.polygonscan.com/address/0x6E35fEE26edeAb6E23EfC98112b8857D132420ac" target="_blank">0x6E35fEE26ede</a></td>
               <td>Website</td>
               <td>$300.00</td>
            </tr>
            <tr class="item">
               <td>Hosting</td>
               <td><a href="https://mumbai.polygonscan.com/address/0x6E35fEE26edeAb6E23EfC98112b8857D132420ac" target="_blank">0x6E35fEE26ede</a></td>
               <td>Hosting</td>
               <td>$75.00</td>
            </tr>
            <tr class="item last">
               <td>Domain name</td>
               <td><a href="https://mumbai.polygonscan.com/address/0x6E35fEE26edeAb6E23EfC98112b8857D132420ac" target="_blank">0x6E35fEE26ede</a></td>
               <td>Domain name</td>
               <td>$10.00</td>
            </tr>
            <tr class="total">
               <td></td>
               <td></td>
               <td></td>
               <td>Total: $385.00</td>
            </tr>
         </table>
      </div>
   </body>
</html>
  `;

  return htmlContent;
}

async function bufferToBase64(buffer: any) {
  return Buffer.from(buffer).toString('base64');
}
export async function generatePdf(req: any, res: any) {
  // const input = req.body;

  try {
    const htmlContent = await getHtmlContent({ id: 0 });

    // const pdfBuffer = await convertToPdf(htmlContent);
    // const pdfBase64 = await bufferToBase64(pdfBuffer);
    const file = { content: htmlContent };
    const options = { format: 'A4' };

    await html_to_pdf
      .generatePdf(file, options)
      .then(async (pdfBuffer: any) => {
        console.log('PDF Buffer:-', pdfBuffer);
        const pdfBase64 = await bufferToBase64(pdfBuffer);
        res.status(200).json({ data: pdfBase64 });
      });

    res.status(401).json({ message: 'no data found' });
    // const pdfBuffer = await pdfAsync(htmlContent, { format: 'A4' });

    // let base64String;
    // console.log({ pdfBuffer });
    // // If in Node.js environment
    // if (typeof window === 'undefined') {
    //   const Buffer = require('buffer').Buffer;
    //   base64String = Buffer.from(pdfBuffer).toString('base64');
    // } else {
    //   // For browser environment
    //   base64String = btoa(String.fromCharCode(...new Uint8Array(pdfBuffer)));
    // }
    // // const pdfBase64 = pdfBuffer.toString('base64');
    // // res.status(200).json({ pdfBase64 });
    // console.log({ base64String });
  } catch (err: any) {
    res.status(500).send({ message: err.message as string });
  }
}
