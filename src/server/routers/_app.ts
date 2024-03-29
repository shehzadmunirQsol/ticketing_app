import { router } from '../trpc';

import { adminUserRouter } from './adminUser';
import { customerUserRouter } from './customer';
import { dashboardRouter } from './dashboard';
import { projectRouter } from './project';
import { rolesRouter } from './userRoles';

export const appRouter = router({
  admin: adminUserRouter,
  dashboard: dashboardRouter,

  roles: rolesRouter,
  customer: customerUserRouter,
  project: projectRouter,
});

export type AppRouter = typeof appRouter;

export const serverRouter = (ctx: any) => appRouter.createCaller(ctx);
