import React, { ReactNode } from 'react';
import Header from './header';
import { useDispatch, useSelector } from 'react-redux';
import { RootState } from '~/store/store';
import Footer from './footer';
import { trpc } from '~/utils/trpc';
import { addCart } from '~/store/reducers/cart';
import { Toaster } from '~/components/ui/toaster';
import { userAuth } from '~/store/reducers/auth';

type DefaultLayoutProps = { children: ReactNode };

function Index({ children }: DefaultLayoutProps) {
  const { lang } = useSelector((state: RootState) => state.layout);
  const { user, isLogin } = useSelector((state: RootState) => state.auth);

  const dispatch = useDispatch();

  trpc.customer.get.useQuery(undefined, {
    refetchOnWindowFocus: false,
    onSuccess(data) {
      if (data?.data) {
        dispatch(userAuth(data?.data));
      }
      console.log({ data });
    },
    onError(error) {
      console.log({ error });
    },
  });

  trpc.cart.get.useQuery(
    { customer_id: user?.id },
    {
      enabled: isLogin,
      refetchOnWindowFocus: false,

      onSuccess(data) {
        const cart = {
          id: data.data?.id ?? null,
          customer_id: user?.id ?? null,
          isDiscount: data?.data?.CouponApply?.length ? true : false,
          discount: data?.data?.CouponApply[0]?.discount ?? 0,
          isPercentage: data?.data?.CouponApply[0]?.is_percentage ?? false,
          cartItems: data.data?.CartItems ?? [],
        };

        dispatch(addCart({ cart, count: 0, totalAmount: 0 }));
      },
      onError(error) {
        console.log({ error });
      },
    },
  );

  return (
    <div
      dir={lang.dir}
      lang={lang.lang}
      className="relative w-full overflow-x-hidden"
    >
      <Toaster />
      <Header />
      {children}
      <Footer />
    </div>
  );
}

export default Index;
