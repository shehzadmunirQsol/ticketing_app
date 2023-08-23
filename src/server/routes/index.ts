import openRoutes from './baseRoutes/open.routes';
// import NextCors from 'nextjs-cors';

async function mainRoutes(req: any, res: any) {
  const allowedDomains = [process.env.NEXT_PUBLIC_LIVE_URL as string];
  const referer: string = req.headers?.referer ?? '';
  const isAllowed = allowedDomains.some((domain) => referer?.includes(domain));

  return openRoutes(req, res);
}

export default mainRoutes;
