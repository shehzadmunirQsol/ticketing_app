import React, { ReactNode } from 'react';
import Header from './header';
import Sidebar from './sidebar';

type DefaultLayoutProps = { children: ReactNode };

function index({ children }: DefaultLayoutProps) {
  return (
    <div className="relative">
      <Header />
      <div className="flex">
        <Sidebar />
        <div className="flex-1">{children}</div>
      </div>
    </div>
  );
}

export default index;
