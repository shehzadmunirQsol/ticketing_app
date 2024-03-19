import { createProject } from '~/server/clientControllers/project/create';
import { getProjectAll } from '~/server/clientControllers/project/get';
import { generateProjectInvoice } from '~/server/clientControllers/project/invoice';
import { getTruckersAll } from '~/server/clientControllers/trucker/get';
import { inviteUser } from '~/server/clientControllers/user/invite';
import { verifyJWT } from '~/utils/jwt';

export default async function projectRoutes(req: any, res: any) {
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
    case 'GET-project/getAll':
      return getProjectAll(req, res);
    case 'POST-project/create':
      return createProject(req, res);
    case 'POST-project/invoice':
      return generateProjectInvoice(req, res);
    case 'GET-project/getTruckers':
      return getTruckersAll(req, res);
    case 'POST-project/inviteUser':
      return inviteUser(req, res);

    default:
      return res.status(405).send({ message: 'This request is not allowed' });
  }
}
