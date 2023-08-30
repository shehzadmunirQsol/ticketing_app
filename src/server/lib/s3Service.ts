import { S3 } from 'aws-sdk';
import { v4 as uuidv4 } from 'uuid';
import { isValidImageType } from '~/utils/helper';

export const s3Upload = async (file: any) => {
  const s3 = new S3();
  try {
    const params: any = {
      Bucket: process.env.AWS_BUCKET_NAME,
      Key: `upload/${uuidv4()}-${file.originalname}`,
      Body: file.buffer,
      // ACL: "public-read",
    };
    return s3.upload(params).promise();
  } catch (error) {
    console.log(error);
  }
};

// const storage = multer.diskStorage({
//   destination: "./public/uploads",
//   filename: (req, file, cb) => cb(null, file.originalname),
// });

export function runMiddleware(req: any, res: any, fn: any) {
  return new Promise((resolve, reject) => {
    fn(req, res, (result: any) => {
      if (result instanceof Error) {
        return reject(result);
      }

      return resolve(result);
    });
  });
}

const region = process.env.AWS_REGION;
const accessKeyId = process.env?.AWS_ACCESS_KEY;
const secretAccessKey = process.env?.AWS_SECRET_KEY;

const s3 = new S3({
  region,
  accessKeyId,
  secretAccessKey,
  signatureVersion: 'v4',
});

export async function generateUploadUrl(req: any, res: any) {
  try {
    const isImage = isValidImageType(req.body?.fileType);

    const direcoryName = isImage ? 'upload' : 'temp';
    const fileName = `${direcoryName}/${uuidv4()}-${req.body.fileName}`;

    const params = {
      Bucket: process.env.AWS_BUCKET_NAME,
      Key: fileName,
      Expires: 60,
    };

    const uploadUrl = await s3.getSignedUrlPromise('putObject', params);
    console.log({ uploadUrl });
    return res.status(200).send({ url: uploadUrl });
  } catch (err: any) {
    res.status(500).send({ message: err.message as string });
  }
}
