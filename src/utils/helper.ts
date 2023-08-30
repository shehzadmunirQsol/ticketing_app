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


export const sendEmail = async (mailOptions:any) => {
  console.log(mailOptions,"mailOptions")
  const res = await fetch(
    `${process.env.NEXT_PUBLIC_BASE_URL}/api/email/mailer`,
    {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify({
        ...mailOptions
      }),
    },
  );

  const { error } = await res.json();
  if (error) {
    console.log(error.response, error.response.body, 'api brevo error');
    return;
  }
}

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




