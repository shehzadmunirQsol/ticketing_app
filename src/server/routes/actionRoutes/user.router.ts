import { getUser } from '~/server/clientControllers/user/get';
import { getUserPermision } from '~/server/clientControllers/user/permision';
import { getUserRole } from '~/server/clientControllers/user/role';
import { verifyJWT } from '~/utils/jwt';

export default async function userRoutes(req: any, res: any) {
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
    case 'GET-user/me':
      return getUser(req, res);
    case 'GET-user/role':
      return getUserRole(req, res);
    case 'GET-user/permisions':
      return getUserPermision(req, res);

    default:
      return res.status(405).send({ message: 'This request is not allowed' });
  }
}
