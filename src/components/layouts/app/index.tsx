import React, { ReactNode } from 'react';
import Header from './header';

type DefaultLayoutProps = { children: ReactNode };

function index({ children }: DefaultLayoutProps) {
  return (
    <>
      <div className="relative w-full overflow-x-hidden">
        <Header />
        <div className="w-full">{children}</div>
      </div>
    </>
  );
}

export default index;
