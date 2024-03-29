const AdminSessionName = 'ticketing-admin-token';
export function getAdminToken(headers: Headers | null) {
  const cookieStore = headers?.get('cookie');
  const ls = cookieStore?.split('; ');
  const token = ls?.find((x) => x.startsWith('ticketing-admin-token='));
  if (!token) return null;
  return token?.replace('ticketing-admin-token=', '');
}

export function getToken(headers: Headers | null) {
  const cookieStore = headers?.get('cookie');
  const ls = cookieStore?.split('; ');
  const token = ls?.find((x) => x.startsWith('winnar-token='));
  if (!token) return null;
  return token?.replace('winnar-token=', '');
}
export function setAdminToken(token: string) {
  window.localStorage.setItem(AdminSessionName, token);
}
