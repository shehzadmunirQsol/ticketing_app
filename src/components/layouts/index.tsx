import { ThemeProvider } from 'next-themes';
import Head from 'next/head';
import { ReactNode } from 'react';
import { Provider } from 'react-redux';
import store from '~/store/store';
import AppLayout from './app';
import AdminLayout from './admin';
import { useRouter } from 'next/router';

type DefaultLayoutProps = { children: ReactNode };

export default function DefaultLayout({ children }: DefaultLayoutProps) {
  const router = useRouter();

  console.log('router', router);

  const Layout = router.asPath.includes('admin') ? AdminLayout : AppLayout;

  return (
    <Provider store={store}>
      <ThemeProvider attribute="class" defaultTheme="dark">
        <Head>
          <title>Winnar</title>
          <link rel="icon" href="/favicon.ico" />
        </Head>
        <Layout>
          <main>{children}</main>
        </Layout>
      </ThemeProvider>
    </Provider>
  );
}
