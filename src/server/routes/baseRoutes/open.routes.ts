import s3UrlRoutes from '../actionRoutes/s3Url.routes';
import totalRoutes from '../actionRoutes/totalProcessing.routes';
import authRoutes from '../actionRoutes/user-auth.router';
import projectRoutes from '../actionRoutes/project.router';

export default async function openRoutes(req: any, res: any) {
  const { query } = req;

  console.log({ query });

  switch (query.routes[0]) {
    case 's3-url':
      return s3UrlRoutes(req, res);
    case 'total-payments':
      return totalRoutes(req, res);
    case 'auth':
      return authRoutes(req, res);
    case 'project':
      return projectRoutes(req, res);

    default:
      return res.status(405).send({ message: 'This request is not allowed' });
  }
}
