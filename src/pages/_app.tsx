import type { NextPage } from 'next';
import type { AppType, AppProps } from 'next/app';
import type { ReactElement, ReactNode } from 'react';
import Layout from '~/components/layouts';
import { trpc } from '~/utils/trpc';
import '~/styles/globals.css';



export type NextPageWithLayout<
  TProps = Record<string, unknown>,
  TInitialProps = TProps,
> = NextPage<TProps, TInitialProps> & {
  getLayout?: (page: ReactElement) => ReactNode;
};

type AppPropsWithLayout = AppProps & {
  Component: NextPageWithLayout;
};

const MyApp = (({ Component, pageProps }: AppPropsWithLayout) => {
  const getLayout =
    Component.getLayout ??
    ((page) => (
      <main className={'font-sans'}>
        <Layout>{page}</Layout>
      </main>
    ));

  return getLayout(
    <main className={'font-sans'}>
      <Component {...pageProps} />
    </main>,
  );
}) as AppType;

export default trpc.withTRPC(MyApp);
