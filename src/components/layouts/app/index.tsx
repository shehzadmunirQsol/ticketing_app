import React, { ReactNode, useEffect, useState } from 'react';
import Header from './header';
import { useDispatch, useSelector } from 'react-redux';
import { RootState } from '~/store/store';
import Footer from './footer';
import { trpc } from '~/utils/trpc';
import { CartItemInterface, addCart } from '~/store/reducers/cart';
import { Toaster } from '~/components/ui/toaster';
import { userAuth } from '~/store/reducers/auth';
import { LoadingDialog } from '~/components/common/modal/loadingModal';
import { useRouter } from 'next/router';
import Link from 'next/link';
import { Button } from '~/components/ui/button';

type DefaultLayoutProps = { children: ReactNode };

function Index({ children }: DefaultLayoutProps) {
  const { lang } = useSelector((state: RootState) => state.layout);
  const { user, isLogin } = useSelector((state: RootState) => state.auth);

  const router = useRouter();
  const dispatch = useDispatch();

  const routesWithoutNavbarAndFooter = '/order-view';

  const shouldShowNavbarAndFooter = !router.pathname.startsWith(
    routesWithoutNavbarAndFooter,
  );

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
        const cartString = localStorage.getItem('winnar-cart');
        if (cartString?.includes('"cartItems":[{"')) {
          createCartHandler(cartString, data.data?.id);
        } else if (data?.data) {
          const cart = {
            id: data.data?.id ?? null,
            customer_id: user?.id ?? null,
            isDiscount: data?.data?.CouponApply?.length ? true : false,
            discount: data?.data?.CouponApply[0]?.discount ?? 0,
            isPercentage: data?.data?.CouponApply[0]?.is_percentage ?? false,
            cartItems: data.data?.CartItems ?? [],
          };

          dispatch(addCart(cart));
        }
      },
      onError(error) {
        console.log({ error });
      },
    },
  );

  const createCart = trpc.cart.createCart.useMutation();

  useEffect(() => {
    const cartString = localStorage.getItem('winnar-cart');
    if (cartString) {
      const cart = JSON.parse(cartString);
      dispatch(addCart(cart));
    }
  }, [dispatch]);

  async function createCartHandler(
    cartString: string,
    cart_id: number | undefined,
  ) {
    try {
      const cartData = JSON.parse(cartString);
      const cartItems = cartData?.cartItems?.map((item: CartItemInterface) => ({
        event_id: item.event_id,
        quantity: item.quantity,
        is_subscribe: item.is_subscribe,
        subscription_type: item.subscription_type,
      }));

      const payload = {
        cart_id: cart_id ?? 0,
        customer_id: user?.id,
        cart_items: cartItems,
      };
      const response = await createCart.mutateAsync(payload);

      const cart = {
        id: response?.data?.id ?? null,
        customer_id: user?.id ?? null,
        isDiscount: response?.data?.CouponApply?.length ? true : false,
        discount: response?.data?.CouponApply[0]?.discount ?? 0,
        isPercentage: response?.data?.CouponApply[0]?.is_percentage ?? false,
        cartItems: response?.data?.CartItems ?? [],
      };

      dispatch(addCart(cart));
      localStorage.removeItem('winnar-cart');
    } catch (error: any) {
      console.log({ error });
    }
  }

  return (
    <div
      dir={lang.dir}
      lang={lang.lang}
      className="relative mx-auto max-w-[1600px] w-full overflow-x-hidden"
    >
      <Toaster />
      {shouldShowNavbarAndFooter ? (
        <>
          <Header />
          {children}
          <LoadingDialog open={createCart.isLoading} text={'Loading...'} />
          <Footer />
          <CookiesLabel />
        </>
      ) : (
        <>{children}</>
      )}
    </div>
  );
}

export default Index;

function CookiesLabel() {
  const [isAccepted, setIsAccepted] = useState(true);

  useEffect(() => {
    const isCookieAccepted =
      localStorage.getItem('winnar-cookies') === 'accepted';

    setIsAccepted(isCookieAccepted);
  }, []);

  function setCookiesHandler() {
    setIsAccepted(true);
    localStorage.setItem('winnar-cookies', 'accepted');
  }

  return (
    <div
      className={`${
        isAccepted ? 'hidden' : 'block'
      } z-[999999] shadow-lg w-full fixed bottom-0 bg-background-footer space-y-2 px-4 py-4 sm:px-14`}
    >
      <h3 className="text-lg sm:text-xl">Cookies on this site</h3>
      <p>
        We use cookies to provide a better user experience. By using our site,
        you agree to our use of cookies. See our{' '}
        <Link className="text-primary" href="/privacy-policy">
          Privacy Policy
        </Link>{' '}
        to learn more.
      </p>
      <div className="flex gap-2">
        <Button onClick={() => setIsAccepted(true)} variant="secondary">
          Close
        </Button>
        <Button onClick={setCookiesHandler}>Accept All Cookies</Button>
      </div>
    </div>
  );
}
