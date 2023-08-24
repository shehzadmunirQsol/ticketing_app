/**
 * This file contains the root router of your tRPC-backend
 */
import { router } from '../trpc';
import { categoryRouter } from './category';
import { eventRouter } from './event';
import { languageRouter } from './language';
import { settingRouter } from './settings';
import { customerRouter } from './customer';

export const appRouter = router({
  category: categoryRouter,
  event: eventRouter,
  language: languageRouter,
  settings: settingRouter,
  customer: customerRouter,
});

export type AppRouter = typeof appRouter;
