import {
  loginCustomer,
  registerCustomer,
} from '~/server/clientControllers/auth';

export default async function authRoutes(req: any, res: any) {
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
