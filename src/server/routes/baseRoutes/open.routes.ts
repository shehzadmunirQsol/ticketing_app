import emailRoutes from '../actionRoutes/email.routes';
import s3UrlRoutes from '../actionRoutes/s3Url.routes';
import totalRoutes from '../actionRoutes/totalProcessing.routes';

export default async function openRoutes(req: any, res: any) {
  const { query } = req;

  console.log({ query });

  switch (query.routes[0]) {
    case 's3-url':
      return s3UrlRoutes(req, res);
    case 'total-payments':
      return totalRoutes(req, res);
    case 'email':
        return emailRoutes(req, res);

    default:
      return res.status(405).send({ message: 'This request is not allowed' });
  }
}
