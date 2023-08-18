export async function getS3Url(file: any) {
  console.log('file::', file);
  const fileName = file?.name
    ?.replaceAll(' ', '-')
    .replaceAll("'", '')
    .replaceAll('&', 'and');
  const fileType = file?.type;

  const params = {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
    },
    body: JSON.stringify({ fileName, fileType }),
  };

  const url = `${process.env.NEXT_PUBLIC_BASE_URL}/api/s3-url`;

  try {
    const data = await (await fetch(url, params)).json();

    return data;
  } catch (error) {
    console.log({ error });
  }
}
export async function uploadToS3Bucket(url: string, file: any) {
  const params = {
    method: 'PUT',
    headers: {
      'Content-Type': 'multipart/form-data',
    },
    body: file,
  };

  try {
    await fetch(url, params);
    return { success: true, error: false };
  } catch (error) {
    console.log({ error });
    return { success: false, error };
  }
}
export async function getS3ImageUrl(file: any) {
  const response = {
    data: '',
    success: true,
    error: false,
    message: 'Uploaded Successfully',
  };

  try {
    const s3Data = await getS3Url(file);
    if (!s3Data.url) {
      response.success = false;
      response.error = s3Data?.error;
      response.message = 'Issue getting url';
      return response;
    }

    const uploadResponse = await uploadToS3Bucket(s3Data.url, file);
    if (!uploadResponse.success) {
      response.success = false;
      response.error = uploadResponse?.error as boolean;
      response.message = 'Something went wrong!';
      return response;
    }
    const imageUrl = s3Data.url
      .split('?')[0]
      .replace('https://s3.amazonaws.com/media.xoltanmarketplace.com/', '');
    // const imageUrl = s3Data.url.split('?')[0];

    response.data = imageUrl;

    return response;
  } catch (error) {
    response.success = false;
    response.error = error as boolean;
    response.message = 'Something went wrong';

    return response;
  }
}
