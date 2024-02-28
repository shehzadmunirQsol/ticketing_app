import {
  loginCustomer,
  registerCustomer,
} from '~/server/clientControllers/auth';

export default async function authRoutes(req: any, res: any) {
  if (!req.headers.authorization) {
    return res.status(401).json({ error: 'Authorization header missing' });
  }
  // Split the Authorization header to get the bearer token
  const [bearer, token] = req.headers.authorization.split(' ');

  // Check if the authorization scheme is Bearer and if the token exists
  if (bearer !== 'Bearer' || !token) {
    return res.status(401).json({ error: 'Invalid authorization format' });
  }
  if (process.env.AUTH_TOKEN !== token) {
    return res.status(401).json({ error: 'Invalid token' });
  }

  const { method, query } = req;

  const extendedAction = `${method}-${query.routes.join('/')}`;

  switch (extendedAction) {
    case 'POST-auth/login':
      return loginCustomer(req, res);
    case 'POST-auth/register':
      return registerCustomer(req, res);

    default:
      return res.status(405).send({ message: 'This request is not allowed' });
  }
}
