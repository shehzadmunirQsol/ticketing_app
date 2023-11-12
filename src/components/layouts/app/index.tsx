import React, { ReactNode, useEffect, useState } from 'react';
import Header from './header';
import { useDispatch, useSelector } from 'react-redux';
import { RootState } from '~/store/store';
import Footer from './footer';
import { trpc } from '~/utils/trpc';
import {
  CartItemInterface,
  addCart,
  setCartLoaded,
} from '~/store/reducers/cart';
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

  const [userToken, setUserToken] = useState<string | null>(null);

  const router = useRouter();
  const dispatch = useDispatch();

  const routesWithoutNavbarAndFooter = '/order-view';

  const shouldShowNavbarAndFooter = !router.pathname.startsWith(
    routesWithoutNavbarAndFooter,
  );

  trpc.customer.get.useQuery(undefined, {
    refetchOnWindowFocus: false,
    enabled: userToken ? true : false,
    onSuccess(data) {
      if (data?.data) {
        dispatch(userAuth(data?.data));
      }
    },
    onError(error) {
      if (error.message === 'jwt malformed') {
        setUserToken(null);
        localStorage.removeItem('winnar-token');
      }
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
        dispatch(setCartLoaded());
      },
      onError(error) {
        console.log({ error });
      },
    },
  );

  const createCart = trpc.cart.createCart.useMutation();

  useEffect(() => {
    const cartString = localStorage.getItem('winnar-cart');
    if (cartString?.includes('"cartItems":[{"')) {
      const cart = JSON.parse(cartString);
      dispatch(addCart(cart));
    } else {
      localStorage.removeItem('winnar-cart');
    }
    setUserToken(localStorage.getItem('winnar-token'));
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
          <SignUpSticky / >
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
      className={`${ isAccepted ? 'hidden' : 'block' } cookiesec`}>
      <div className="mycontainer px-2 md:px-12">
        <div className="row align-items-center">
          <div className="col">  
            <h3>Cookie Policy</h3>
            <p>Should you choose to proceed in accessing this website, non-permanent cookies will be placed on your computer to enhance your experience whilst using the site. If you do not wish to have such non-permanent cookies placed on your computer please exit the site now. Alternatively, please click Accept All Cookies to proceed.</p>     
          </div>
          <div className="col-auto mt-3 md:mt-0">  
          <div className="winbtn winbtnormal font-sans"  onClick={setCookiesHandler} role="button" tabIndex={0}>
            Accept All Cookies
          </div>
          {/* <div className="winbtn winbtnormal font-sans" onClick={() => setIsAccepted(true)} role="button" tabIndex={0}>
            Close
          </div>  */}
          </div>
        </div>
      </div>
    </div>
  );
}


function SignUpSticky() {
  const [isSignup, setIsSignup] = useState(true);

  useEffect(() => {
    const isSignupCookieAccepted =
      localStorage.getItem('signup-cookies') === 'accepted';
      setIsSignup(isSignupCookieAccepted);
  }, []);

  function setSignupHandler() {
    setIsSignup(true);
    localStorage.setItem('signup-cookies', 'accepted');
  }

  return (
    <div className={`${ isSignup ? 'hidden' : 'block' } cookiesec`}>


<div className="mycontainer px-2 md:px-12">
        <div className="row align-items-center justify-content-center">
          <div className="col-auto">  
            <h3>SignUp</h3>
            <p>Please Signup for Winnar.</p>     
          </div>
          <div className="col-auto mt-3 md:mt-0">  
        <Link href="/login" className="winbtn winbtnormal font-sans">
        Signup
        </Link>
          <div className="winbtn winbtnormal font-sans"  onClick={setSignupHandler} role="button" tabIndex={0}>
            Close
          </div>
          </div>
        </div>
      </div>
    </div>
  );
}