/**
 * This file contains the root router of your tRPC-backend
 */
import { publicProcedure, router } from '../trpc';
// import { postRouter } from './post';
import { adminUserRouter } from './adminUser';

export const appRouter = router({
  healthcheck: publicProcedure.query(() => 'yay!'),
  user: adminUserRouter,
  // post: postRouter,
});

export type AppRouter = typeof appRouter;
