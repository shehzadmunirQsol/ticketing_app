import { useRouter } from 'next/router';
import { ReactNode, useEffect, useState } from 'react';
import { useDispatch, useSelector } from 'react-redux';
import {
  userAdminAuth,
  userAdminIsLogin,
} from '~/store/reducers/adminAuthSlice';
import { RootState } from '~/store/store';
import { trpc } from '~/utils/trpc';

const protectedRoutes = ['/admin/login', '/admin/register'];

export default function AdminAuth({ children }: { children: ReactNode }) {
  const [loading, setLoading] = useState(true);
  const dispatch = useDispatch();

  const router = useRouter();
  const { isLogin } = useSelector((state: RootState) => state.adminAuth);
  const {
    data: userData,
    isLoading,
    isFetched,
    isFetching,
    isFetchedAfterMount,
    isError,
  } = trpc.admin.me.useQuery(
    { isLogin },
    {
      refetchOnMount: true,
      refetchOnWindowFocus: false,

      onSuccess(data) {
        dispatch(userAdminAuth(data as any));
        dispatch(userAdminIsLogin(data ? true : false));
      },
    },
  );
  useEffect(() => {
    const isProtectedRoute = protectedRoutes.includes(router.pathname);
    const timeout = setTimeout(() => {
      if (!isLoading) {
        if (
          isProtectedRoute &&
          userData &&
          userData != null &&
          isFetchedAfterMount
        ) {
          console.log('i am login');
          router.push('/admin/dashboard');
          setLoading(false);
        } else if (
          !isProtectedRoute &&
          isFetchedAfterMount &&
          (isError || !userData)
        ) {
          console.log('i am logout');
          router.push('/admin/login');
          setLoading(false);
        }
        setLoading(false);
      }
    }, 1000);

    return () => {
      if (timeout) clearTimeout(timeout);
    };
  }, [isFetched, isLoading, userData, typeof window !== 'undefined']);

  return (
    <>
      {loading ? (
        <div className="flex h-screen w-screen flex-col items-center justify-center">
          <div className="loader ease-linear rounded-full border-4 border-t-4 border-gray-200 h-12 w-12 mb-4"></div>
          <h2 className="text-white text-xl font-semibold">Loading...</h2>
        </div>
      ) : (
        <>{children}</>
      )}
    </>
  );
}
