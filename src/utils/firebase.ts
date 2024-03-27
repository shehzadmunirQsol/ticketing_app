// Import the functions you need from the SDKs you need
import { initializeApp } from 'firebase/app';
import { getFirestore } from 'firebase/firestore';
import { getMessaging } from 'firebase/messaging';
// import { getMessaging, getToken } from "firebase/messaging";
import admin from 'firebase-admin';

const firebaseConfig = {
  apiKey: process.env.NEXT_PUBLIC_FIREBASE_API_KEY,
  authDomain: process.env.NEXT_PUBLIC_FIREBASE_AUTH_DOMAIN,
  projectId: process.env.NEXT_PUBLIC_FIREBASE_PROJECT_ID,
  storageBucket: process.env.NEXT_PUBLIC_FIREBASE_STORAGE_BUCKET_ID,
  messagingSenderId: process.env.NEXT_PUBLIC_FIREBASE_MESSAGING_SERNDER_ID,
  appId: process.env.NEXT_PUBLIC_FIREBASE_APP_ID,
  measurementId: process.env.NEXT_PUBLIC_FIREBASE_MEASUREMENT_ID,
};

// Initialize Firebase

export const app = initializeApp(firebaseConfig);
export const db = getFirestore();
if (!admin.apps.length) {
  admin.initializeApp({
    credential: admin.credential.cert({
      // Your Firebase service account credentials here
      // Make sure to securely store these credentials
      // For example, you can use environment variables
      projectId: 'ticketing-25ba6',
      privateKey:
        '-----BEGIN PRIVATE KEY-----\nMIIEvQIBADANBgkqhkiG9w0BAQEFAASCBKcwggSjAgEAAoIBAQCfAgJh0SeeMklI\ntYWQkKyVyMPC5gXvGgg1UIa/hUfETTWgXpSPzJcoMm5Z/pou0yUHJGi528DSOfGX\n6tzRz32HjZDo3yUuOizq3AjdyMs9vCremQnZgvZCmmavYz8d3ajcLvST/rAy1BOQ\n5OQuZ8gXuRaTkk1Jjcfbg8FNejNTXgjUKcLtc8tR5zHmHg51RXA6uc0N3OcFUcSI\nIX6H8ZIxG31CqB4o9JMKMakNAZ7ZB/rHpWpcXk5Xq4/v8M1Mepm9yKNi2ltrZ6tU\n6l3jS7MOgRJrdBlFG/MaUDjfcrbTJPcadLbZFrRZ3wl3YA3MAXTSwewx6ixhOrEN\ny1XU6YZLAgMBAAECggEAALdVGb9ZPX9Te4DZylC505jft4EqtgIjgaihsBDNGxy6\nk5opcsuvQ0Pq4aaMZ46VjvEtND0W+BDBE6KVU6/qtvwTvYDCcvFZzMWol+XKHeE6\n6N1pQRyzuDwSyf0ZRoH+OzOjmrEIdnTPERV+05itutxKC5ManFdc9ueighNSp4Wg\nlNBWBSzQ5/Zanamma+wDyOHuy+dK3L2H/Pl8zyYZD/iLdyYtSyw2UYFbyGrCgntM\nAFCLL4pOxrdgBDtXroOIyspTTT7ahgs9zd0E810YtsMbbJMVwX1rw87eJf3s/yD9\nndKgQ2BxIU/G6X5qI30R7hvpb8ZziQ7kLViRmL0T1QKBgQDPB8+FShgP48SumZz0\nl5CWZEPYoXJotxPj9ic6ClnBwmbLMOimZTZm0cZkRFMoDyMj48JrONh0LNyzY4D5\nJAYR6Zz/XEMfzPLu6MDsm6892EY/hHxkQRjBVo2YC4AeJc/jTTVOuESypE7CdtWQ\n4EOSMQPbGpAqqrcGQBtipS6eJwKBgQDEnk/6H4Zy6cof1U7eZKC5Qht0fkNg7u4t\nCGLSLU2e77JwIWFqKaV2VL39twJeAWy7eStFm1yGc4fUXGxvPc5SEDOn3ftRY/jX\ny9z94oduQFvot2cDliUWY/PQN8SjnZZYoBT6jltTBCKoe9lMSofkyhCQ21DGcFN4\n30D8BtDRPQKBgQCKb6+dclk6rtSC6CcjCwJc/ji77+EgEmPTiHLchbVVMhgogNDh\nC0bgZB+kRmfQnqahxhfoOL/Ml38Q2VWRwzvn2G3p95jO9+3uQWdYEBaTZT48FOxU\n4Y6bIbRW2kHWZvRnCua9d8xAJEyiDnJqiPvGEs6AQXfrf/IY1N87DDdq+wKBgHW3\nJsDkgPVpvWbDqd2CN6vSxWdAdQyi4Bw8ChY1o2RFs/poMc+CsZqf78Pn/tXTWor/\nZ4XcF1Az+R2OXClZwp3lL1gQkffPt9tTJbMnHdhHVf+FVnqSORAntYBy8xrWLX5Q\nEV9VPZ39OtbOl3GSqbuzsqlXdQvpkkAqwdhiKEEtAoGAf8whONptMJFXhydlTsvB\nD9HLsJVyxBuXOcwi8ZxtAeP6NmfDOv34OOU38ctLonQj0GLtuJW0JDwMfDs0EC9V\nZxZW8R6i6hdEqILiFAc3HKsShTq8ijNwCzeLplUIWJGaT0zilDqROvnYijWh7WKu\n6XrLpXqVllTD5yIZMsMlPxo=\n-----END PRIVATE KEY-----\n',
      clientEmail:
        'firebase-adminsdk-kkny0@ticketing-25ba6.iam.gserviceaccount.com',
    }),
  });
}
