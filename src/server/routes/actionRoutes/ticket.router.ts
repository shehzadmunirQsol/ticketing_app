import { createTicket } from '~/server/clientControllers/ticket/create';
import { getAllTicket } from '~/server/clientControllers/ticket/get';
import { updateTicket } from '~/server/clientControllers/ticket/update';
import { verifyJWT } from '~/utils/jwt';

export default async function ticketRoutes(req: any, res: any) {
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
    case 'GET-ticket/getAll':
      return getAllTicket(req, res);
    //   case 'GET-project/getTruckers':
    //     return getTruckersAll(req, res);
    //   case 'POST-project/inviteUser':
    //     return inviteUser(req, res);
    case 'POST-ticket/create':
      return createTicket(req, res);
    case 'POST-ticket/update':
      return updateTicket(req, res);

    default:
      return res.status(405).send({ message: 'This request is not allowed' });
  }
}
