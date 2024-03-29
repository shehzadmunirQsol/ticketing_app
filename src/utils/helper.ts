import { verifyJWT } from './jwt';

export function generateOTP(otp_length = 0) {
  const digits = '0123456789';
  let OTP = '';
  for (let i = 0; i < otp_length; i++) {
    OTP += digits[Math.floor(Math.random() * 10)];
  }
  return OTP;
}

export function formatTrpcError(trpcError = 'Something went wrong!' as string) {
  if (trpcError?.includes('[\n  {\n  ')) {
    const formattedError = JSON.parse(trpcError);
    const msgError =
      formattedError?.length > 0
        ? formattedError[0].message
        : 'Internal server error';
    return msgError;
  } else {
    return trpcError;
  }
}
export function renderImage(data: any) {
  console.log({ data });
  return data?.media_type === 'audio/mp3' ||
    data?.thumb === '' ||
    data?.profile_pic === '' ||
    !data?.profile_pic
    ? 'https://www.vhv.rs/dpng/d/15-155087_dummy-image-of-user-hd-png-download.png'
    : `${data?.profile_pic}`;
}

export function isValidImageType(type: any) {
  const isImage = type?.includes('image/') && type !== 'image/gif';
  return isImage;
}

type EmailOptionsType = {
  from: string;
  to: string;
  subject: string;
  template_id: number;
  params?: object;
};

export const sendEmail = async (mailOptions: EmailOptionsType) => {
  try {
    const options = {
      method: 'POST',
      headers: {
        accept: 'application/json',
        'content-type': 'application/json',
        'api-key': process.env.NEXT_PUBLIC_BREVO_EMAIL_API_KEY as string,
      },
      body: JSON.stringify({
        sender: { name: 'Winnar', email: mailOptions.from },
        to: [{ email: mailOptions.to }],
        subject: mailOptions.subject,
        templateId: mailOptions.template_id,
        params: { ...mailOptions.params },
        //   params: {FNAME: 'HASSAN', LNAME: 'SHAN'},
      }),
    };
    const res = await fetch('https://api.brevo.com/v3/smtp/email', options);

    if (!res.ok) {
      console.log('email did not send');
      return;
    }
  } catch (error) {
    console.log(error);
  }
};

type SMSOptionsType = {
  to: string;
  subject: string;
};

export async function compressImage(fileImage: File, fileType = 'image/webp') {
  const bitmap = await createImageBitmap(fileImage);
  const canvas = document.createElement('canvas');
  const ctx = canvas.getContext('2d');
  canvas.width = bitmap.width;
  canvas.height = bitmap.height;
  ctx?.drawImage(bitmap, 0, 0);
  // Convert canvas content to a new Blob with reduced quality
  const reducedBlob: Blob = await new Promise((resolve) => {
    canvas.toBlob((blob) => resolve(blob as Blob), fileType, 0.5);
  });

  // Create a new File object from the reduced Blob
  const reducedFile = new File([reducedBlob], fileImage.name, {
    type: fileType, // Adjust the type if needed
    lastModified: fileImage.lastModified,
  });

  return reducedFile;
}

export function customTruncateHandler(str = '', n = 15) {
  return str?.length > n ? str?.slice(0, n) + '...' : str;
}
export function customTruncate(str = '', n = 15) {
  return str?.length > n ? str?.slice(0, n) + '' : str;
}
export function customEmailTruncateHandler(str = '', n = 15) {
  const myArray: any = str.split('@');
  return myArray[0]?.length > n
    ? myArray[0]?.slice(0, n) + '***@' + myArray[1]
    : str;
}

export const displayDate = (payload = '' as any) => {
  if (!payload) return 'N/A';
  const date = new Date(payload);
  return date?.toDateString();
  // const ye = new Intl.DateTimeFormat('en', { year: 'numeric' })?.format(date);
  // const mo = new Intl.DateTimeFormat('en', { month: 'short' })?.format(date);
  // const da = new Intl.DateTimeFormat('en', { day: '2-digit' })?.format(date);
  // const formattedDate = `${da}-${mo}-${ye}`;
  // return formattedDate;
};
export function isValidEmail(email: any) {
  const pattern = /^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$/;
  return pattern.test(email);
}

export async function createSlug(input: any) {
  const lowercaseInput = input?.toLowerCase();
  const cleanedInput = lowercaseInput.replace(/[^\w\s-]/g, '');
  const slug = cleanedInput.replace(/\s+/g, '-');
  return slug.replace(/^-+|-+$/g, '');
}

type AvailableTicketsType = {
  event: {
    total_tickets: number;
    tickets_sold: number;
    user_ticket_limit: number;
  };
  ticketPurchased: number;
  quantity: number;
};

export function getAvailableTickets({
  event,
  ticketPurchased,
  quantity,
}: AvailableTicketsType) {
  const availableTickets = event?.total_tickets - (event?.tickets_sold ?? 0);

  const userTicketLimit =
    availableTickets > event?.user_ticket_limit - ticketPurchased
      ? event?.user_ticket_limit - ticketPurchased
      : availableTickets;

  const isTicketLimit = quantity == userTicketLimit;
  const isTicketLimitExceeded = quantity > userTicketLimit;

  return {
    userTicketLimit,
    availableTickets,
    isTicketLimit,
    isTicketLimitExceeded,
  };
}

export function URIGenerator(title = '' as string, id = 0 as number) {
  const url = `${title?.replace(new RegExp(' ', 'g'), '-')}-${id}`;

  return encodeURI(url);
}

export function URIDecoder(url = '' as any) {
  const decodedURI = decodeURI(url ?? '') ?? '';
  const splittedUrl = decodedURI?.split('-');

  const id = splittedUrl[splittedUrl.length - 1] ?? '';
  const title = decodedURI?.substring(0, decodedURI?.length - (id?.length + 2));
  return { id, title };
}

export const EMAIL_TEMPLATE_IDS = {
  REGISTRATION_OTP: 2,
  CONTACT_MAIN: 4,
  FORGET_PASSWORD: 5,
  SELECT_WINNER: 7,
  ORDER_SUCCESS: 30,
  ORDER_FAILED: 9,
  NEW_REGISTERED_USER: 10,
};

export const priceTranslator = (price: number, lang = 'en') => {
  return price.toLocaleString(`${lang}-EG`);
};

export const EMAILS = {
  contact: 'contact@app.winnar.com',
};
export const validateEmail = (email: string): boolean => {
  const emailParts = email.split('@');
  if (emailParts.length !== 2) {
    // Ensure there's exactly one "@" symbol in the email address
    return false;
  }

  const domain = emailParts[1];
  const domainParts = domain?.split('.');
  if (
    domainParts &&
    ['google'].find((item) => domainParts.includes(item))
    // ['google', 'yahoo', 'outlook'].find((item) => domainParts.includes(item))
  ) {
    // Ensure there's at least one subdomain and a top-level domain
    return false;
  }

  if (domainParts && domainParts.length < 2) {
    // Ensure there's at least one subdomain and a top-level domain
    return false;
  }

  // Check for duplicates in the domainParts array
  const uniqueDomainParts = new Set(domainParts);
  if (domainParts && uniqueDomainParts.size !== domainParts.length) {
    // If there are duplicates, return false
    return false;
  }

  return true;
};
export const validateRegixEmail = (email: string): boolean => {
  const regex = /^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+.[a-zA-Z]{2,}$/;
  return regex.test(email);
};

export function reduceVATAmount(amount: number | undefined) {
  if (amount) {
    return amount ? amount / (1 + 0.05) : 0;
  } else {
    return 0;
  }
}
export function getVATAmount(amount: number | undefined) {
  if (amount) {
    const reducedAmount = reduceVATAmount(amount);
    return reducedAmount * 0.05;
  } else {
    return 0;
  }
}

export function numberToWords(amount: number): string {
  const units: string[] = [
    '',
    'one',
    'two',
    'three',
    'four',
    'five',
    'six',
    'seven',
    'eight',
    'nine',
  ];
  const teens: string[] = [
    '',
    'eleven',
    'twelve',
    'thirteen',
    'fourteen',
    'fifteen',
    'sixteen',
    'seventeen',
    'eighteen',
    'nineteen',
  ];
  const tens: string[] = [
    '',
    'ten',
    'twenty',
    'thirty',
    'forty',
    'fifty',
    'sixty',
    'seventy',
    'eighty',
    'ninety',
  ];

  const convert = (num: number): string | undefined => {
    if (num === 0) {
      return 'zero';
    } else if (num < 10) {
      return units[num];
    } else if (num < 20) {
      return teens[num - 11];
    } else if (num < 100) {
      return tens[Math.floor(num / 10)] + ' ' + units[num % 10];
    } else if (num < 1000) {
      return units[Math.floor(num / 100)] + ' hundred ' + convert(num % 100);
    } else if (num < 1000000) {
      return (
        convert(Math.floor(num / 1000)) + ' thousand ' + convert(num % 1000)
      );
    } else {
      return undefined; // Extend for larger numbers if needed
    }
  };
  const result = convert(amount);
  if (result === undefined) {
    console.log('Unable to convert the given number to words.');
    return 'N/A';
  }
  return result;
}

export function getQueryParameter(name: any) {
  if (typeof window !== 'undefined') {
    const urlParams = new URLSearchParams(window.location.search);
    return urlParams.get(name);
  } else {
    return '';
  }
}
export function stringToBoolean(str: string) {
  // Convert string to lowercase and check if it's 'true'
  return str.toLowerCase() === 'true';
}
export async function getUserData(req: any, res: any) {
  if (!req.headers.authorization) {
    return res.status(401).json({ error: 'Authorization header missing' });
  }
  const [bearer, token] = req.headers.authorization.split(' ');

  // Check if the authorization scheme is Bearer and if the token exists
  if (bearer !== 'Bearer' || !token) {
    return res.status(401).json({ error: 'Invalid authorization format' });
  }
  const userData = await verifyJWT(token);
  return userData;
}

export async function getHtmlContent(data: any) {
  console.log('date', data);
  const date = new Date(data?.updated_at);

  const formattedDate = date.toLocaleDateString('en-US', {
    month: 'short',
    day: '2-digit',
    year: 'numeric',
  });
  let pickAddressString = '';
  let dropAddressString = '';
  const projectAddresses = data?.ProjectAddress;

  // Iterate through the project addresses
  for (const address of projectAddresses) {
    // Check if the address type is "pick"
    if (address.address_type === 'pick') {
      pickAddressString = `${address.street_address_1} ${address.street_address_2}`;
    }
    // Check if the address type is "drop"
    else if (address.address_type === 'drop') {
      dropAddressString = `${address.street_address_1} ${address.street_address_2}`;
    }
  }
  console.log('Pick Address:', pickAddressString);
  console.log('Drop Address:', dropAddressString);

  const clientData = data?.Client;
  const userData = data?.User;
  // Extract usernames or fallback to first names if usernames are not available
  const clientUsername = clientData.username || clientData.first_name || 'N/A';
  const userUsername = userData.username || userData.first_name || 'N/A';
  const FinalizedData = {
    date: formattedDate,
    invoiceid: data?.id,
    from: data?.Client.email,
    to: data?.User.email,
    fromusername: clientUsername,
    tousername: userUsername,
    fromAddress: pickAddressString,
    toAddress: dropAddressString,
  };

  const projectData = data;
  function generateTicketRows() {
    const tickets = projectData.ProjectTickets || [];
    let rows = '';

    tickets.forEach(
      (ticket: {
        Trucker: { username: any; first_name: any };
        id: number;
        trucker_id: any;
        tx_hash: any;
        status: any;
      }) => {
        const trucker = ticket.Trucker || {};
        const truckerName = trucker.username || trucker.first_name || 'N/A';

        rows += `
              <tr class="item">
                  <td>${ticket.id}</td>
                  <td><a href="https://mumbai.polygonscan.com/tx/${
                    ticket.tx_hash
                  }" target="_blank">${customTruncateHandler(
          ticket.tx_hash,
          15,
        )}</a></td>
                  <td>${truckerName}</td>
                  <td>${ticket.status}</td>
              </tr>
          `;
      },
    );
    return rows;
  }
  console.log(formattedDate);
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
                           Invoice #: ${FinalizedData.invoiceid}<br />
                           Created: ${FinalizedData.date}
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
                           From: ${FinalizedData.from}.<br />
                          ${FinalizedData.fromusername}<br />
                          ${FinalizedData.fromAddress}
                        </td>
                        <td>
                           To: ${FinalizedData.to}.<br />
                           ${FinalizedData.tousername}<br />
                           ${FinalizedData.toAddress}
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
            ${generateTicketRows()}
         </table>
      </div>
   </body>
</html>
  `;

  return htmlContent;
}
