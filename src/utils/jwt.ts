import jwt from 'jsonwebtoken';

const SECRET = process.env.TOKEN_SECRET || 'T3TUT';

export function signJWT(data: object) {
  return jwt.sign(data, SECRET);
}

export function verifyJWT<T>(token: string) {
  return jwt.verify(token, SECRET) as T;
}
