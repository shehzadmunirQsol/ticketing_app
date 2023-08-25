import React, { ReactNode } from 'react';
import { Button } from '~/components/ui/button';
import Header from './header';
import { useSelector } from 'react-redux';
import { RootState } from '~/store/store';
import Footer from './footer';
import { Toaster } from '~/components/ui/toaster';

type DefaultLayoutProps = { children: ReactNode };

function Index({ children }: DefaultLayoutProps) {

  const { lang } = useSelector((state: RootState) => state.layout);


  return (
    <div
      dir={lang.dir}
      lang={lang.lang}
      className="relative w-full overflow-x-hidden"
    >
      <Toaster />
      <Header />
      <div className="w-full ">{children}</div>
      <Footer />

    </div>
  );
}

export default Index;
