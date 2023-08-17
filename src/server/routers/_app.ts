/**
 * This file contains the root router of your tRPC-backend
 */
import { publicProcedure, router } from '../trpc';
import { settingRouter } from './settings';
// import { postRouter } from './post';

export const appRouter = router({
  healthcheck: publicProcedure.query(() => 'yay!'),

  // post: postRouter,
  settings: settingRouter,
});

export type AppRouter = typeof appRouter;
