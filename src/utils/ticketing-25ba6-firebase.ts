export const serviceAccount = {
  type: 'service_account',
  project_id: process.env.NEXT_PUBLIC_FIREBASE_PROJECT_ID,
  private_key_id: 'ef2c79d620198b5292c7e1e51c921a35d0e83f54',
  private_key: process.env?.FIREBASE_PRIVATE_KEY,
  client_email: process.env?.FIREBASE_CLIENT_EMAIL,
  client_id: '104601546701075966492',
  auth_uri: 'https://accounts.google.com/o/oauth2/auth',
  token_uri: 'https://oauth2.googleapis.com/token',
  auth_provider_x509_cert_url: 'https://www.googleapis.com/oauth2/v1/certs',
  client_x509_cert_url:
    'https://www.googleapis.com/robot/v1/metadata/x509/firebase-adminsdk-kkny0%40ticketing-25ba6.iam.gserviceaccount.com',
  universe_domain: 'googleapis.com',
};
