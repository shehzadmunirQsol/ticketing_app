import React, { ReactNode } from 'react';
import Header from './header';
import { useSelector } from 'react-redux';
import { RootState } from '~/store/store';

type DefaultLayoutProps = { children: ReactNode };

function Index({ children }: DefaultLayoutProps) {
  const { lang } = useSelector((state: RootState) => state.layout);

  return (
    <div
      dir={lang.dir}
      lang={lang.lang}
      className="relative w-full overflow-x-hidden"
    >
      <Header />
      <div className="w-full">{children}</div>
    </div>
  );
}

export default Index;
