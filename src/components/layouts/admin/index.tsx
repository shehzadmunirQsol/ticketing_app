import React, { ReactNode, useEffect } from 'react';
import Header from './header';
import Sidebar from './sidebar';
import { useRouter } from 'next/router';
import { Toaster } from '~/components/ui/toaster';
import { RootState } from '~/store/store';
import { useDispatch, useSelector } from 'react-redux';
import {
  userAdminAuth,
  userAdminIsLogin,
} from '~/store/reducers/adminAuthSlice';
import { trpc } from '~/utils/trpc';

type DefaultLayoutProps = { children: ReactNode };

function AdminLayout({ children }: DefaultLayoutProps) {
  const { isLogin } = useSelector((state: RootState) => state.adminAuth);
  const { isSidebarOpen } = useSelector(
    (state: RootState) => state.adminLayout,
  );
  const router = useRouter();
  const dispatch = useDispatch();

  const { data } = trpc.admin.me.useQuery(
    { isLogin },
    {
      refetchOnWindowFocus: false,
      enabled: typeof window !== 'undefined' ? true : false,
    },
  );

  useEffect(() => {
    console.log('Data: ', data);
    if (data?.id) {
      dispatch(userAdminAuth(data as any));
      dispatch(userAdminIsLogin(true));
    }
    // do not update this dependancy
  }, [dispatch, data]);

  return (
    <div className="relative ">
      {router.asPath === '/admin/login' ? (
        <main className="flex-1 m-auto">{children}</main>
      ) : (
        <>
          <Header />

          <div className="flex max-h-[calc(100vh-65px)] overflow-hidden">
            <Sidebar />
            <main
              className={`flex-1 w-full ${
                isSidebarOpen ? 'w-[calc(100vw-15%)]' : 'w-[calc(100vw-80px)]'
              }  min-h-[calc(100vh-100px)] overflow-y-scroll`}
            >
              {children}
            </main>
          </div>
        </>
      )}
      <Toaster />
    </div>
  );
}

export default AdminLayout;
