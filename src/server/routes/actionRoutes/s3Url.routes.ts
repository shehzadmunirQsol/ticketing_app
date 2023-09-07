import { generateUploadUrl } from '~/server/lib/s3Service';

export default async function orderRoutes(req: any, res: any) {
  const { method, query } = req;
  const extendedAction = `${method}-${query.routes.join('/')}`;

  switch (extendedAction) {
    case 'POST-s3-url':
      return generateUploadUrl(req, res);

    default:
      return res.status(405).send({ message: 'This request is not allowed' });
  }
}
