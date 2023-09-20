import { NextResponse } from 'next/server';
import type { NextRequest } from 'next/server';
import { getAdminToken, getToken } from '~/utils/authToken';

export function middleware(request: NextRequest) {
  // Clone the request headers and set a new header x-hello-from-middleware1
  if (request.nextUrl.pathname.startsWith('/admin')) {
    const storeRequestHeaders = new Headers(request.headers);

    const isProtectedAdminRoutes = ['/admin/login'].includes(
      request.nextUrl.pathname,
    );
    const token = getAdminToken(storeRequestHeaders);
    if (isProtectedAdminRoutes && token)
      return NextResponse.redirect(new URL('/admin/dashboard', request.url));
    if (!isProtectedAdminRoutes && !token)
      return NextResponse.redirect(new URL('/admin/login', request.url));
  }

  if (request.nextUrl.pathname.startsWith('/')) {
    const storeRequestHeaders = new Headers(request.headers);

    const isProtectedRoutes = ['/checkout', '/account'].includes(
      request.nextUrl.pathname,
    );
    const token = getToken(storeRequestHeaders);
    if (request.nextUrl.pathname === '/login' && token)
      return NextResponse.redirect(new URL('/', request.url));
    if (isProtectedRoutes && !token)
      return NextResponse.redirect(new URL('/login', request.url));
    return NextResponse.next();
  }
  return NextResponse.next();
}
