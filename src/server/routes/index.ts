import openRoutes from './baseRoutes/open.routes';
import NextCors from 'nextjs-cors';

async function mainRoutes(req: any, res: any) {
  await NextCors(req, res, {
    // Options
    methods: ['GET', 'HEAD', 'PUT', 'PATCH', 'POST', 'DELETE'],
    origin: '*',
    optionsSuccessStatus: 200, // some legacy browsers (IE11, various SmartTVs) choke on 204
  });

  return openRoutes(req, res);
}

export default mainRoutes;
