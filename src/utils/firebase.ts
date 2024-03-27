// Import the functions you need from the SDKs you need
import { initializeApp } from 'firebase/app';
import { getFirestore } from 'firebase/firestore';
import { getMessaging } from 'firebase/messaging';
import admin from 'firebase-admin';
const serviceAccount = require('~/utils/ticketing-25ba6-firebase.json');

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
    // credential: admin.credential.cert({
    //   projectId: process.env.NEXT_PUBLIC_FIREBASE_PROJECT_ID,
    //   clientEmail: process.env?.FIREBASE_CLIENT_EMAIL,
    //   privateKey: process.env?.FIREBASE_PRIVATE_KEY?.replace(/\\n/g, '\n'),
    // }),
    credential: admin.credential.cert(serviceAccount),
  });
}
