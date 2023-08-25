import { ThemeProvider } from 'next-themes';
import Head from 'next/head';
import { ReactNode } from 'react';
import { Provider } from 'react-redux';
import store from '~/store/store';
import AppLayout from './app';
import AdminLayout from './admin';
import { useRouter } from 'next/router';

import Icon from '~/public/assets/favicon.png';

// import { Toaster } from "../ui/toaster"
import toast, { Toaster } from 'react-hot-toast';
type DefaultLayoutProps = { children: ReactNode };

export default function DefaultLayout({ children }: DefaultLayoutProps) {
  const router = useRouter();

  console.log('router', router);

  const Layout = router.asPath.includes('admin') ? AdminLayout : AppLayout;

  return (
    <Provider store={store}>
      <Toaster 
        position="top-right"
        reverseOrder={false}/>
      <ThemeProvider attribute="class" defaultTheme="dark">
        <Head>
          <title>Winnar</title>
          <link rel="icon" href={Icon.src} />
        </Head>
        <Layout>{children}</Layout>
      </ThemeProvider>
    </Provider>
  );
}
