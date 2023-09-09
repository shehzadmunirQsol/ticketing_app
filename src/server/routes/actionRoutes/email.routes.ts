import Emailhandler from '~/server/clientControllers/email';
import { generateUploadUrl } from '~/server/lib/s3Service';

export default async function emailRoutes(req: any, res: any) {
  const { method, query } = req;
  const extendedAction = `${method}-${query.routes.join('/')}`;

  switch (extendedAction) {
    case 'POST-email/mailer':
      return Emailhandler(req, res);

    default:
      return res.status(405).send({ message: 'This request is not allowed' });
  }
}
