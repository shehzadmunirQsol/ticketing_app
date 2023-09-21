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
import { paymentRouter } from './payment';
import { orderRouter } from './order';
import { cmsRouter } from './cms';
import { subscriptionRouter } from './subscription';
import { dashboardRouter } from './dashboard';
import { NextApiRequest, NextApiResponse } from 'next';

export const appRouter = router({
  admin: adminUserRouter,
  cms: cmsRouter,
  category: categoryRouter,
  event: eventRouter,
  language: languageRouter,
  settings: settingRouter,
  customer: customerRouter,
  cart: cartRouter,
  winner: winnerRouter,
  contact: contactRouter,
  coupon: couponRouter,
  payment: paymentRouter,
  order: orderRouter,
  subscription: subscriptionRouter,
  dashboard: dashboardRouter,
});

export type AppRouter = typeof appRouter;

export const serverRouter = (ctx: any) => appRouter.createCaller(ctx);
