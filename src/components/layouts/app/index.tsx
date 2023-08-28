import React, { ReactNode } from 'react';
import Header from './header';
import { useDispatch, useSelector } from 'react-redux';
import { RootState } from '~/store/store';
import Footer from './footer';
import { trpc } from '~/utils/trpc';
import { addCart } from '~/store/reducers/cart';

type DefaultLayoutProps = { children: ReactNode };

function Index({ children }: DefaultLayoutProps) {
  const { lang } = useSelector((state: RootState) => state.layout);

  const dispatch = useDispatch();

  trpc.cart.get.useQuery(
    { customer_id: 1 },
    {
      enabled: true,

      onSuccess(data) {
        dispatch(addCart({ cart: data.data }));
        console.log({ data });
      },
      onError(error) {
        console.log({ error });
      },
    },
  );

  return (
    <div dir={lang.dir} lang={lang.lang}>
      <Header />
      {children}
      <Footer />
    </div>
  );
}

export default Index;
