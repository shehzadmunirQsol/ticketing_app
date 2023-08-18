import React, { ReactNode } from 'react';
import Header from './header';
import Sidebar from './sidebar';
import { useRouter } from 'next/router';
import { Toaster } from '~/components/ui/toaster';

type DefaultLayoutProps = { children: ReactNode };

function AdminLayout({ children }: DefaultLayoutProps) {
  const router = useRouter();

  return (
    <div className="grid min-h-screen">
      {router.asPath === '/admin/login' ? (
        <main className="flex-1 m-auto">{children}</main>
      ) : (
        <>
          <Header />
          <div className="flex">
            <Sidebar />
            <main className="flex-1">{children}</main>
          </div>
        </>
      )}
      <Toaster />
    </div>
  );
}

export default AdminLayout;
