import { NextResponse } from 'next/server';
import type { NextRequest } from 'next/server';
import { getAdminToken, getToken } from '~/utils/authToken';

export function middleware(request: NextRequest, requestHeaders: Headers) {
  // Clone the request headers and set a new header x-hello-from-middleware1
  if (request.nextUrl.pathname.startsWith('/admin')) {
    const storeRequestHeaders = new Headers(request.headers);

    const isProtecedAdminRoutes = ['/admin/login'].includes(
      request.nextUrl.pathname,
    );
    const token = getAdminToken(storeRequestHeaders);
    console.log('TOken: ', token);
    if (isProtecedAdminRoutes && token)
      return NextResponse.redirect(new URL('/admin/dashboard', request.url));
    if (!isProtecedAdminRoutes && !token)
      return NextResponse.redirect(new URL('/admin/login', request.url));
  }

  // if (request.nextUrl.pathname.startsWith('/')) {
  //   const storeRequestHeaders = new Headers(request.headers);

  //   const isProtecedAdminRoutes = ['/login'].includes(request.nextUrl.pathname);
  //   const token = getToken(storeRequestHeaders);
  //   console.log('TOken111: ', token);
  //   if (isProtecedAdminRoutes && token)
  //     return NextResponse.redirect(new URL('/', request.url));
  //   if (!isProtecedAdminRoutes && !token)
  //     return NextResponse.redirect(new URL('/login', request.url));
  //   return NextResponse.next();
  // }
  return NextResponse.next();
}
