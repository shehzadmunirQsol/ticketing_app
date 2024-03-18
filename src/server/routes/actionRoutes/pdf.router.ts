import { getSearchedClient } from '~/server/clientControllers/client';
import { generatePdf } from '~/server/clientControllers/pdf';
import {
  createProject,
  getProjectAll,
} from '~/server/clientControllers/project';
import { verifyJWT } from '~/utils/jwt';

export default async function pdfRoutes(req: any, res: any) {
  const { method, query } = req;
  const extendedAction = `${method}-${query.routes.join('/')}`;

  switch (extendedAction) {
    case 'GET-generate-pdf':
      return generatePdf(req, res);
    case 'POST-project/create':
      return createProject(req, res);

    default:
      return res.status(405).send({ message: 'This request is not allowed' });
  }
}
