/**
 * This file contains the root router of your tRPC-backend
 */
import { router } from '../trpc';
import { categoryRouter } from './category';
import { eventRouter } from './event';

export const appRouter = router({
  category: categoryRouter,
  event: eventRouter,
});

export type AppRouter = typeof appRouter;
