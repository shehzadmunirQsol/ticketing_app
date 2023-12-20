import { getCookie } from 'cookies-next';
import { NextResponse } from 'next/server';
import type { NextRequest } from 'next/server';
// import { getAdminToken, getToken } from '~/utils/authToken'; 

export function middleware(request: NextRequest, response: NextResponse) {
  
  if (request.nextUrl.pathname.startsWith('/admin')) {
    // const storeRequestHeaders = new Headers(request.headers);
    // const token = getAdminToken(storeRequestHeaders);

    const token = getCookie('winnar-admin-token', {
      req: request,
      res: response,
    });

    const isProtectedAdminRoutes = ['/admin/login'].includes(
      request.nextUrl.pathname,
    );

    if (isProtectedAdminRoutes && token)
      return NextResponse.redirect(new URL('/admin/dashboard', request.url));
    if (!isProtectedAdminRoutes && !token)
      return NextResponse.redirect(new URL('/admin/login', request.url));
  }

  if (request.nextUrl.pathname.startsWith('/')) {
    // const storeRequestHeaders = new Headers(request.headers);
    // const token = getToken(storeRequestHeaders);
    const token = getCookie('winnar-token', { req: request, res: response });

    // const isProtectedRoutes = ['/checkout', '/account'].includes(
    //   request.nextUrl.pathname,
    // );
    const isProtectedRoutes = ['/account'].includes(
      request.nextUrl.pathname,
    );
    const isProtectedCheckout = ['/checkout'].includes(
      request.nextUrl.pathname,
    );

    if (request.nextUrl.pathname === '/login' && token)
      return NextResponse.redirect(new URL('/', request.url));
    if (request.nextUrl.pathname.includes('/reset-password') && token)
      return NextResponse.redirect(new URL('/', request.url));
    if (isProtectedRoutes && !token){
      return NextResponse.redirect(new URL('/login', request.url));
    }
    if (isProtectedCheckout && !token){
      // return NextResponse.redirect(new URL('/login?redirect=checkout', request.url));
      return NextResponse.redirect(new URL('/login', request.url));
    }


    // const isProtectedCarPage = request.nextUrl.pathname.includes('/cars/');
    // const isProtectedCashPage = request.nextUrl.pathname.includes('/cash/');
    // if ((isProtectedCarPage || isProtectedCashPage) && !token){
    //   return NextResponse.redirect(new URL('/login', request.url));
    // }


      
    return NextResponse.next();
  }
  return NextResponse.next();
}
