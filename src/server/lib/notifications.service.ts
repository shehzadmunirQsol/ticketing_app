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
import { Messaging, getMessaging } from 'firebase/messaging';
import { v4 as uuidv4 } from 'uuid';
import { app, db } from '~/utils/firebase';

// export const notificationMessageTypes = {
//   OFFER_RECEIVED: "offer-received",
//   OFFER_ACCEPTED: "offer-accepted",
//   OFFER_REJECTED: "offer-rejected",
//   OFFER_EXPIRING: "offer-expiring",
//   OFFER_EXPIRED: "offer-expired",
//   OFFER_NFT_PURCHASED: "offer-nft-purchased",
//   OFFER_NFT_SOLD: "offer-nft-sold",
//   NFT_PURCHASED: "nft-purchased",
//   NFT_SOLD: "nft-sold",
//   NFT_REPORTED: "nft-reported",
//   NFT_REPORTED_APPROVED: "nft-reported-approved",
//   NFT_REPORTED_REJECTED: "nft-reported-rejected",
//   NFT_LIKED: "nft-liked",
//   NFT_BLOCKED: "nft-blocked",
// };

// export async function getOrCreateNotifications(wallet_address = "") {
//   const response = {
//     unsub: null,
//     notifications: null,
//   };

//   if (!wallet_address) return response;

//   try {
//     response.unsub = onSnapshot(
//       doc(db, "notifications", wallet_address),
//       async (document) => {
//         if (document.data()) {
//           console.log(document.data());
//           response.notifications = document.data();
//         } else {
//           await setDoc(doc(db, "notifications", wallet_address), {
//             // notifications: arrayUnion(), ## also a correct way.
//             notifications: [],
//           });
//         }
//       }
//     );

//     console.log("I Ran");
//     return response;
//   } catch (err) {
//     console.log(err);
//     return response;
//   }
// }

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
  const PushMessage = {
    data: {
      score: '850',
      time: '2:45',
    },
    token: '',
  };
  // const messaging = app.options.messagingSenderId.;

  // make sure document_id is valid, string and wallet_address.
  console.log({ docData, params });
  const pushMessage: any = getMessaging(app);
  pushMessage.app().sendToDevice({
    data: {
      score: '850',
      time: '2:45',
    },
    token:
      'fEMMUNeQSuy6V0yIzwtNmF:APA91bHN6Upgyk3UbervvELgNEQ2Y-2PzJgO1wedG-wTCLPSoBq1dPP5kTc9CGfEB5B0nBWINOJh1NvNIKWjwORd4CSk0kOTQ4wlvf_j1EQ4G6iiyZKm7mWIWOPFvaFaYaX2Rhw65nv1',
  });
  // pushMessage.sendToDevice({
  //   PushMessage: {
  //     data: {
  //       score: '850',
  //       time: '2:45',
  //     },
  //     token: 'registrationToken',
  //   },
  // });

  const ownersNotification = setDoc(
    doc(db, 'notifications', document_id, 'notifications', id),
    docData,
  );

  return ownersNotification;
}
