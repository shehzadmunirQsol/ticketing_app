import { cookies } from 'next/headers';

const AdminSessionName = 'winnar-admin-token';
export function getAdminToken(headers:Headers | null){
    console.log(headers);
    const cookieStore = headers?.get('cookie');
    const ls = cookieStore?.split('; ');
    const token = ls?.find(x => x.startsWith('winnar-token='));
    if (!token) return null;
    return token?.replace('winnar-token=','');
}

export function setAdminToken(token: string){
    window.localStorage.setItem(AdminSessionName, token);
}