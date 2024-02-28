import { getSearchedClient } from '~/server/clientControllers/client';
import {
  createProject,
  getProjectAll,
} from '~/server/clientControllers/project';
import { verifyJWT } from '~/utils/jwt';

export default async function clientRoutes(req: any, res: any) {
  if (!req.headers.authorization) {
    return res.status(401).json({ error: 'Authorization header missing' });
  }
  // Split the Authorization header to get the bearer token
  const [bearer, token] = req.headers.authorization.split(' ');

  // Check if the authorization scheme is Bearer and if the token exists
  if (bearer !== 'Bearer' || !token) {
    return res.status(401).json({ error: 'Invalid authorization format' });
  }
  const userData = await verifyJWT(token);
  if (!userData) {
    return res.status(401).json({ error: 'Invalid token' });
  }
  const { method, query } = req;
  const extendedAction = `${method}-${query.routes.join('/')}`;

  switch (extendedAction) {
    case 'GET-client/search':
      return getSearchedClient(req, res);
    case 'POST-project/create':
      return createProject(req, res);

    default:
      return res.status(405).send({ message: 'This request is not allowed' });
  }
}
