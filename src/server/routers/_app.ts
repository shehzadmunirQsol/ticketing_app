/**
 * This file contains the root router of your tRPC-backend
 */

  // post: postRouter,
import { router } from '../trpc';
import { categoryRouter } from './category';
import { eventRouter } from './event';
import { languageRouter } from './language';
import { settingRouter } from './settings';
import { adminUserRouter } from './adminUser';

export const appRouter = router({
  admin: adminUserRouter,
  category: categoryRouter,
  event: eventRouter,
  language: languageRouter,
  settings: settingRouter,
});

export type AppRouter = typeof appRouter;
