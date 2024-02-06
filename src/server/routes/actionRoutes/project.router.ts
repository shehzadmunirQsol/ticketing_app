import {
  createProject,
  getProjectAll,
} from '~/server/clientControllers/project';

export default async function projectRoutes(req: any, res: any) {
  const { method, query } = req;
  const extendedAction = `${method}-${query.routes.join('/')}`;

  switch (extendedAction) {
    case 'GET-project/getAll':
      return getProjectAll(req, res);
    case 'POST-project/create':
      return createProject(req, res);

    default:
      return res.status(405).send({ message: 'This request is not allowed' });
  }
}
