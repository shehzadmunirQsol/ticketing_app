import authRoutes from '../actionRoutes/user-auth.router';
import projectRoutes from '../actionRoutes/project.router';
import clientRoutes from '../actionRoutes/client.router';

export default async function openRoutes(req: any, res: any) {
  const { query } = req;

  console.log({ query });

  switch (query.routes[0]) {
    case 'auth':
      return authRoutes(req, res);
    case 'project':
      return projectRoutes(req, res);
    case 'client':
      return clientRoutes(req, res);

    default:
      return res.status(405).send({ message: 'This request is not allowed' });
  }
}
