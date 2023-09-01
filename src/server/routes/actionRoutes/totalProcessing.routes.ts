import { totalProcessingPayment } from '~/server/clientControllers/totalProcessing';
import { generateUploadUrl } from '~/server/lib/s3Service';

export default async function totalRoutes(req: any, res: any) {
  const { method, query } = req;
  const extendedAction = `${method}-${query.routes.join('/')}`;

  switch (extendedAction) {
    case 'POST-total-payments':
      return totalProcessingPayment(req, res);
    case 'POST-total-payments/subscription':
      return generateUploadUrl(req, res);

    default:
      return res.status(405).send({ message: 'This request is not allowed' });
  }
}
