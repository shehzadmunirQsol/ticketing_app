import {compare, hash} from "bcrypt";

export async function isSamePass(unHashPass: string, hashPass: string){
    return await compare(unHashPass, hashPass)
}

export async function hashPass(unHashPass: string) {
    return await hash(unHashPass, 10);
}