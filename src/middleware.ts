import { NextResponse } from 'next/server';
import type { NextRequest } from 'next/server';
import { getAdminToken } from '~/utils/authToken';

export function middleware(request: NextRequest, requestHeaders: Headers) {
  // Clone the request headers and set a new header x-hello-from-middleware1
  const storeRequestHeaders = new Headers(request.headers);
  if (request.nextUrl.pathname.startsWith('/admin')) {
    const isProtecedAdminRoutes = ['/admin/login'].includes(
      request.nextUrl.pathname,
    );
    const token = getAdminToken(storeRequestHeaders);
    console.log("TOken: ",token)
    if (isProtecedAdminRoutes && token)
      return NextResponse.redirect(new URL('/admin/dashboard', request.url));
    if (!isProtecedAdminRoutes && !token)
      return NextResponse.redirect(new URL('/admin/login', request.url));
  }
  return NextResponse.next();
}
