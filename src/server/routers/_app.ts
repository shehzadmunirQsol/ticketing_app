import { router } from '../trpc';

import { adminUserRouter } from './adminUser';

export const appRouter = router({
  admin: adminUserRouter,
});

export type AppRouter = typeof appRouter;

export const serverRouter = (ctx: any) => appRouter.createCaller(ctx);
