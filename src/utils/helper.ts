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
export function renderNFTImage(nft: any) {
  return nft?.media_type === 'audio/mp3' || nft?.thumb === ''
    ? ''
    : `${process.env.NEXT_PUBLIC_MEDIA_BASE_URL}${nft?.thumb}`;
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
  console.log(mailOptions, 'mailOptions');
  try {
    const options = {
      method: 'POST',
      headers: {
        accept: 'application/json',
        'content-type': 'application/json',
        'api-key': process.env.BREVO_EMAIL_API_KEY as string,
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
      console.log(res);
      return console.log('email did not send');
    }
  } catch (error) {
    console.log(error);
  }
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
  console.log(input, 'frontinput');
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
  console.log({ event, ticketPurchased, quantity });
  const availableTickets = event?.total_tickets - (event?.tickets_sold ?? 0);

  const userTicketLimit =
    availableTickets > event?.user_ticket_limit - ticketPurchased
      ? event?.user_ticket_limit - ticketPurchased
      : availableTickets;

  const isTicketLimit = quantity >= userTicketLimit;

  return { userTicketLimit, availableTickets, isTicketLimit };
}

export function URIGenerator(title: string, id: number) {
  const url = `${title?.replaceAll(' ', '-')}-${id}`;
  return encodeURI(url);
}

export function URIDecoder(url: any) {
  console.log({ url });
  const decodedURI = decodeURI(url ?? '');
  console.log({ decodedURI });

  const id = decodedURI?.split('-')?.at(-1) ?? '';
  const title = decodedURI?.substring(0, decodedURI?.length - (id?.length + 2));
  return { id, title };
}

export const EMAIL_TEMPLATE_IDS = {
  REGISTRATION_OTP: 2,
  CONTACT_MAIN: 4,
  FORGET_PASSWORD: 5,
  SELECT_WINNER: 7,
  ORDER_SUCCESS: 8,
  ORDER_FAILED: 9,
  NEW_REGISTERED_USER: 10,
};

export const priceTranslator=(price:number, lang="en")=>{
  return price.toLocaleString(`${lang}-EG`)
}