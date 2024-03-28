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
import { db } from '~/utils/firebase';

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
};

export async function notificationHandler(params: any) {
  const {
    user_id,
    document_id,
    type,
    message,
    device_id,
    title,
    route = '',
  } = params;

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

  const registrationToken =
    device_id ?? (process?.env?.FIREBASE_FCM_TEST as string);

  const PushMessage = {
    notification: {
      title: title,
      body: message,
    },
    to: registrationToken,
  };

  await fetch(`https://fcm.googleapis.com/fcm/send`, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
      Authorization: `Bearer  ${process.env.FIREBASE_SERVER_KEY} `,
    },
    body: JSON.stringify(PushMessage),
  });

  const ownersNotification = setDoc(
    doc(db, 'notifications', document_id, 'notifications', id),
    docData,
  );

  return ownersNotification;
}
