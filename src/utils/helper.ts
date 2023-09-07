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

export const sendEmail = async (mailOptions: any) => {
  console.log(mailOptions, 'mailOptions');
  const res = await fetch(
    `${process.env.NEXT_PUBLIC_BASE_URL}/api/email/mailer`,
    {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify({
        ...mailOptions,
      }),
    },
  );

  const { error } = await res.json();
  if (error) {
    console.log(error.response, error.response.body, 'api brevo error');
    return;
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
export function customEmailTruncateHandler(str = '', n = 15) {
  const myArray: any = str.split('@');
  return myArray[0]?.length > n
    ? myArray[0]?.slice(0, n) + '***@' + myArray[1]
    : str;
}

export const displayDate = (payload = '' as any) => {
  if (!payload) return 'N/A';
  const date = new Date(payload);
  const ye = new Intl.DateTimeFormat('en', { year: 'numeric' })?.format(date);
  const mo = new Intl.DateTimeFormat('en', { month: 'short' })?.format(date);
  const da = new Intl.DateTimeFormat('en', { day: '2-digit' })?.format(date);
  const formattedDate = `${da}-${mo}-${ye}`;
  return formattedDate;
};
export function isValidEmail(email: any) {
  console.log(email, 'email found');
  const pattern = /^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$/;
  return pattern.test(email);
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

export const EMAIL_TEMPLATE_IDS = {
  REGISTRATION_OTP: 2,
  CONTACT_MAIN: 4,
  FORGET_PASSWORD: 5,
  SELECT_WINNER: 7,
  ORDER_SUCCESS: 8,
  ORDER_FAILED: 9,
  NEW_REGISTERED_USER: 10,
};
