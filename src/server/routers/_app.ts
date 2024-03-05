import { router } from '../trpc';

import { adminUserRouter } from './adminUser';
import { customerUserRouter } from './customer';
import { rolesRouter } from './userRoles';

export const appRouter = router({
  admin: adminUserRouter,
  roles: rolesRouter,
  customer: customerUserRouter,
});

export type AppRouter = typeof appRouter;

export const serverRouter = (ctx: any) => appRouter.createCaller(ctx);
