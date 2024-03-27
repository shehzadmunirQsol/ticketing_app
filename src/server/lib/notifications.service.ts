import {
  doc,
  Timestamp,
  setDoc,
  query,
  collection,
  orderBy,
  limit,
  where,
  getCountFromServer,
  getDocs,
  updateDoc,
} from 'firebase/firestore';
import { v4 as uuidv4 } from 'uuid';
import { app, db } from '~/utils/firebase';
import admin from 'firebase-admin';

export const notificationQuery = (user_id: string, limitLength = 5) => {
  return query(
    collection(db, 'notifications', user_id, 'notifications'),
    orderBy('created_at', 'desc'),
    limit(limitLength),
  );
};

export const getUnseenNotificationsCount = async (user_id: string) => {
  const query_ = query(
    collection(db, 'notifications', user_id, 'notifications'),
    where('is_seen', '==', false),
  );
  const snapshot = await getCountFromServer(query_);
  return snapshot;
};

export const updateAllUnseenDocuments = async (user_id: string) => {
  const collectionName = collection(
    db,
    'notifications',
    user_id,
    'notifications',
  );
  const notificationsQueryData = await getDocs(
    query(collectionName, where('is_seen', '==', false)),
  );

  const updateNotifications: any[] = [];

  notificationsQueryData.forEach(async (document) => {
    const notificationRef = doc(
      db,
      'notifications',
      user_id,
      'notifications',
      document.id,
    );

    const updateNotification = updateDoc(notificationRef, { is_seen: true });
    updateNotifications.push(updateNotification);
  });
  await Promise.all(updateNotifications);
};

export const notificationsTypes = {
  PRODUCT: 'product',
  SUCCESS: 'success',
  ALERT: 'alert',
  WARNING: 'warning',
  INFO: 'info',
};

export const notificationsMessages = {
  projectCreated: (data: any) => {
    return `Your project has been created ( ${data?.name} ).`;
  },
  clientInvitation: (data: any) => {
    return `Seller Add you as client in  Project: ( ${data?.name} ).`;
  },
  truckerInvitation: (data: any) => {
    return `Seller Invite you as trucker in project: ( ${data?.name} ).`;
  },
  ticketCreated: (data: any) => {
    return `You have created ticket in project: ( ${data?.project?.name} ).`;
  },
  projectTicketCreated: (data: any) => {
    return `Ticket Created by Trucker:( ${
      data?.userData?.first_name ?? data?.userData?.username
    } ) in following project: ( ${data?.project?.name} ).`;
  },
  offerNFTPurchased: (data: any) => {
    return `Congratulations! you've purchased the NFT ( ${data?.name} )`;
  },
  offerNFTSold: (data: any) => {
    return `You've recieved the payment for NFT ( ${data?.name} )`;
  },
  NFTPurchased: (data: any) => {
    return `Congratulations! you've purchased the NFT ( ${data?.name} )`;
  },
  NFTSold: (data: any) => {
    return `You've recieved the payment for NFT ( ${data?.name} )`;
  },
  NFTReported: (data: any) => {
    return `Your NFT ( ${data?.name} ) has been reported by a customer.`;
  },
  NFTReportedApproved: (data: any) => {
    return `Your Report on NFT ( ${data?.name} ) has been Accepted.`;
  },
  NFTReportedRejected: (data: any) => {
    return `Your Report on NFT ( ${data?.name} ) has been Rejected.`;
  },
  NFTBlocked: (data: any) => {
    return `Your NFT ( ${data?.name} ) has been blocked on our marketplace due to multipe reports`;
  },
  NFTUnBlocked: (data: any) => {
    return `Congratulations! Your NFT ( ${data?.name} ) has been unblocked on our marketplace.`;
  },
  NFTLiked: (data: any, user: any) => {
    return `${user?.name} liked your NFT ( ${data?.name} ).`;
  },
  NFTFeatured: (data: any) => {
    return `Congratulations! Your NFT ( ${data?.name} ) has been featured.`;
  },
  NFTUnFeatured: (data: any) => {
    return `Your NFT ( ${data?.name} ) has been un-featured.`;
  },
};

export async function notificationHandler(params: any) {
  const { user_id, document_id, type, message, route = '' } = params;
  console.log(document_id, 'document_id');

  const id = uuidv4();
  const docData = {
    id,
    route,
    content: message,
    created_at: Timestamp.now(),
    type: type,
    user_id: user_id,
    is_seen: false,
  };

  // const messaging = app.options.messagingSenderId.;

  // make sure document_id is valid, string and wallet_address.
  console.log({ docData, params });

  const registrationToken =
    'fEMMUNeQSuy6V0yIzwtNmF:APA91bHN6Upgyk3UbervvELgNEQ2Y-2PzJgO1wedG-wTCLPSoBq1dPP5kTc9CGfEB5B0nBWINOJh1NvNIKWjwORd4CSk0kOTQ4wlvf_j1EQ4G6iiyZKm7mWIWOPFvaFaYaX2Rhw65nv1';

  const PushMessage = {
    data: {
      title: '850',
      body: '2:45',
    },
    // token: registrationToken,
  };
  const messgaData = await admin
    .messaging()
    .sendToDevice(registrationToken, PushMessage);
  // const messgaData = await admin
  //   .messaging()
  //   .subscribeToTopic(registrationToken, 'PushMessage');
  console.log({ messgaData: messgaData?.results });
  const ownersNotification = setDoc(
    doc(db, 'notifications', document_id, 'notifications', id),
    docData,
  );

  return ownersNotification;
}
