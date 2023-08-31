/**
 * This file contains the root router of your tRPC-backend
 */

// post: postRouter,
import { router } from '../trpc';
import { categoryRouter } from './category';
import { eventRouter } from './event';
import { languageRouter } from './language';
import { settingRouter } from './settings';
import { customerRouter } from './customer';
import { adminUserRouter } from './adminUser';
import { cartRouter } from './cart';
import { winnerRouter } from './winners';
import { contactRouter } from './contact';
import { couponRouter } from './coupon';

export const appRouter = router({
  admin: adminUserRouter,
  category: categoryRouter,
  event: eventRouter,
  language: languageRouter,
  settings: settingRouter,
  customer: customerRouter,
  cart: cartRouter,
  winner: winnerRouter,
contact: contactRouter,
  coupon: couponRouter,
});

export type AppRouter = typeof appRouter;
