import React, { ReactNode } from 'react';
import Header from './header';

type DefaultLayoutProps = { children: ReactNode };

function index({ children }: DefaultLayoutProps) {
  return (
    <>
      <div className="relative">
        <Header />
        <div className="w-screen">{children}</div>
      </div>
    </>
  );
}

export default index;
