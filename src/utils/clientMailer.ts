// email to user for otp
type sendEmailType = {
  from?: string;
  email: string;
  type: string;
  subject: string;
  raw?: string;
};
export async function sendInvitation(props: sendEmailType) {
  const res = await fetch(
    `${process.env.NEXT_PUBLIC_BASE_URL}/api/sendgrid/to_user`,
    {
      method: 'POST',

      headers: {
        'Content-Type': 'application/json',
      },

      body: JSON.stringify({
        email: props?.email,
        type: props?.type,
        subject: props?.subject,
      }),
    },
  );

  const [response] = await Promise.all([res]);
  console.log({ response }, 'responseData');

  const result = await response.json();
  return result;
}
export async function sendLoginEmail(email: string, otp: any) {
  console.log('inside login email', email, otp);
  const res = await fetch(
    `${process.env.NEXT_PUBLIC_BASE_URL}/api/sendgrid/to_user`,
    {
      method: 'POST',

      headers: {
        'Content-Type': 'application/json',
      },

      body: JSON.stringify({
        email: email,
        otp: otp,
        type: 'otp',
        subject: 'Login OTP - Xoltanmarketplace',
      }),
    },
  );

  const { error } = await res.json();
  if (error) {
    console.log(error.response, error.response.body, 'api send grif error');
    return;
  }
}

// email from user contact form
export async function sendStoreContactEmail(data: any, messgae: any) {
  const res = await fetch(
    `${process.env.NEXT_PUBLIC_BASE_URL}/api/sendgrid/to_user`,
    {
      method: 'POST',

      headers: {
        'Content-Type': 'application/json',
      },

      body: JSON.stringify({
        email: data.email,
        title: messgae.title,
        from: messgae.email,
        message: messgae.message,
        subject: 'User Contact - Xoltanmarketplace',
      }),
    },
  );
  console.log({ data, messgae });
  console.log({ res }, 'emailData');

  const [response] = await Promise.all([res]);
  console.log({ response }, 'responseData');

  const result = await response.json();
  return result;
}
export async function sendContactEmail(data: any) {
  const res = await fetch(
    `${process.env.NEXT_PUBLIC_BASE_URL}/api/sendgrid/to_admin`,
    {
      method: 'POST',

      headers: {
        'Content-Type': 'application/json',
      },

      body: JSON.stringify({
        email: data.email,
        title: data.title,
        from: data.email,
        message: data.message,
        subject: 'User Contact - Xoltanmarketplace',
      }),
    },
  );
  console.log({ res }, 'emailData');

  const [response] = await Promise.all([res]);
  console.log({ response }, 'responseData');

  const result = await response.json();
  return result;
}

// email generated for admin when site created
export async function sendCreateWebsiteEmail(data: any) {
  const res = fetch(
    `${process.env.NEXT_PUBLIC_BASE_URL}/api/sendgrid/to_user`,
    {
      method: 'POST',

      headers: {
        'Content-Type': 'application/json',
      },

      body: JSON.stringify({
        ...data,
        subject: 'Website Creation - Xoltanmarketplace',
      }),
    },
  );

  const resAdmin = fetch(
    `${process.env.NEXT_PUBLIC_BASE_URL}/api/sendgrid/to_admin`,
    {
      method: 'POST',

      headers: {
        'Content-Type': 'application/json',
      },

      body: JSON.stringify({
        ...data,
        subject: 'Website Creation - Xoltanmarketplace',
      }),
    },
  );

  const [response, responseAdmin] = await Promise.all([res, resAdmin]);

  const { error } = await response.json();

  if (error) {
    console.log(error.response, error.response.body, 'a[pi send grif error');
    return;
  }
}

// email generated for admin when new user registered
export async function newRegisteredUserEmail(data: any) {
  const res = fetch(
    `${process.env.NEXT_PUBLIC_BASE_URL}/api/sendgrid/to_admin`,
    {
      method: 'POST',

      headers: {
        'Content-Type': 'application/json',
      },

      body: JSON.stringify({
        ...data,
        subject: 'New Registered User - Xoltanmarketplace',
      }),
    },
  );
  console.log({ data }, 'emailData');

  const [response] = await Promise.all([res]);

  const { error } = await response.json();
  if (error) {
    console.log(error.response, error.response.body, 'a[pi send grif error');
    return;
  }
}

// email generated for admin when subcription cancelled   X
export async function cancelSubscription(data: any) {
  const res = fetch(
    `${process.env.NEXT_PUBLIC_BASE_URL}/api/sendgrid/to_user`,
    {
      method: 'POST',

      headers: {
        'Content-Type': 'application/json',
      },

      body: JSON.stringify({
        ...data,
        subject: 'Cancel Subscription - Xoltanmarketplace',
      }),
    },
  );

  const resAdmin = fetch(
    `${process.env.NEXT_PUBLIC_BASE_URL}/api/sendgrid/to_admin`,
    {
      method: 'POST',

      headers: {
        'Content-Type': 'application/json',
      },

      body: JSON.stringify({
        ...data,
        subject: 'Cancel Subscription - Xoltanmarketplace',
      }),
    },
  );

  const [response, responseAdmin] = await Promise.all([res, resAdmin]);

  const { error } = await response.json();
  if (error) {
    console.log(error.response, error.response.body, 'a[pi send grif error');
    return;
  }
}

// email generated for admin when plan purchased    X
export async function planSubscription(data: any) {
  const res = fetch(
    `${process.env.NEXT_PUBLIC_BASE_URL}/api/sendgrid/to_user`,
    {
      method: 'POST',

      headers: {
        'Content-Type': 'application/json',
      },

      body: JSON.stringify({
        ...data,
        subject: 'New Plan Subscriber - Xoltanmarketplace',
      }),
    },
  );

  const resAdmin = fetch(
    `${process.env.NEXT_PUBLIC_BASE_URL}/api/sendgrid/to_admin`,
    {
      method: 'POST',

      headers: {
        'Content-Type': 'application/json',
      },

      body: JSON.stringify({
        ...data,
        subject: 'New Plan Subscriber - Xoltanmarketplace',
      }),
    },
  );

  const [response, responseAdmin] = await Promise.all([res, resAdmin]);

  const { error } = await response.json();

  if (error) {
    console.log(error.response, error.response.body, 'a[pi send grif error');
    return;
  }
}

// email generated for admin when plan changed   X
export async function upgardeSubscription(data: any) {
  const res = fetch(
    `${process.env.NEXT_PUBLIC_BASE_URL}/api/sendgrid/to_user`,
    {
      method: 'POST',

      headers: {
        'Content-Type': 'application/json',
      },

      body: JSON.stringify({
        ...data,
        subject: 'Upgrade Subscription - Xoltanmarketplace',
      }),
    },
  );

  const resAdmin = fetch(
    `${process.env.NEXT_PUBLIC_BASE_URL}/api/sendgrid/to_admin`,
    {
      method: 'POST',

      headers: {
        'Content-Type': 'application/json',
      },

      body: JSON.stringify({
        ...data,
        subject: 'Upgrade Subscription - Xoltanmarketplace',
      }),
    },
  );

  const [response, responseAdmin] = await Promise.all([res, resAdmin]);

  const { error } = await response.json();

  if (error) {
    console.log(error.response, error.response.body, 'a[pi send grif error');
    return;
  }
}

// email from user for entriprise plan
export async function sendRequestEmail(data: any) {
  const res = await fetch(
    `${process.env.NEXT_PUBLIC_BASE_URL}/api/sendgrid/to_admin`,
    {
      method: 'POST',

      headers: {
        'Content-Type': 'application/json',
      },

      body: JSON.stringify({
        ...data,
        subject: 'User Enterprise Plan - Xoltanmarketplace',
      }),
    },
  );

  const resUser = fetch(
    `${process.env.NEXT_PUBLIC_BASE_URL}/api/sendgrid/to_admin`,
    {
      method: 'POST',

      headers: {
        'Content-Type': 'application/json',
      },

      body: JSON.stringify({
        ...data,
        subject: 'User Enterprise Plan - Xoltanmarketplace',
      }),
    },
  );

  const [response, responseUser] = await Promise.all([res, resUser]);

  const { error } = await response.json();

  if (error) {
    console.log(error.response, error.response.body, 'a[pi send grif error');
    return;
  }
}

// email from store admin to store customer for buy nft
export async function sendBuyNFTEmail(data: any) {
  const res = await fetch(
    `${process.env.NEXT_PUBLIC_BASE_URL}/api/sendgrid/to_user`,
    {
      method: 'POST',

      headers: {
        'Content-Type': 'application/json',
      },

      body: JSON.stringify({
        ...data,
        type: 'buy',
        subject: 'Order Purchase - Xoltanmarketplace',
      }),
    },
  );

  const { error } = await res.json();
  if (error) {
    console.log(error.response, error.response.body, 'a[pi send grif error');
    return;
  }
}

// email from store admin to store customer for offer and auction acception
export async function sendOfferEmail(data: any) {
  const res = await fetch(
    `${process.env.NEXT_PUBLIC_BASE_URL}/api/sendgrid/to_user`,
    {
      method: 'POST',

      headers: {
        'Content-Type': 'application/json',
      },

      body: JSON.stringify({
        ...data,
        type: 'offer',
        subject: 'Offer - Xoltanmarketplace',
      }),
    },
  );

  const { error } = await res.json();
  if (error) {
    console.log(error.response, error.response.body, 'a[pi send grif error');
    return;
  }
}

export async function sendCreateWebsiteWaitingEmail(data: any) {
  const res = await fetch(
    `${process.env.NEXT_PUBLIC_BASE_URL}/api/sendgrid/to_user`,
    {
      method: 'POST',

      headers: {
        'Content-Type': 'application/json',
      },

      body: JSON.stringify({
        ...data,
        type: 'create-waiting',
        subject: 'Website Creation Under Process - Xoltanmarketplace',
      }),
    },
  );

  const { error } = await res.json();
  if (error) {
    console.log(error.response, error.response.body, 'a[pi send grif error');
    return;
  }
}

export async function sendNewsletterEmail(data: any) {
  const res = await fetch(
    `${process.env.NEXT_PUBLIC_BASE_URL}/api/sendgrid/to_admin`,
    {
      method: 'POST',

      headers: {
        'Content-Type': 'application/json',
      },

      body: JSON.stringify({
        ...data,
        type: 'newsletter',
        subject: 'Newsletter - Xoltanmarketplace',
      }),
    },
  );

  const resUser = await fetch(
    `${process.env.NEXT_PUBLIC_BASE_URL}/api/sendgrid/to_user`,
    {
      method: 'POST',

      headers: {
        'Content-Type': 'application/json',
      },

      body: JSON.stringify({
        ...data,
        type: 'newsletter',
        subject: 'Newsletter - Xoltanmarketplace',
      }),
    },
  );

  const { error } = await res.json();
  const { error: errorUser } = await resUser.json();
  if (error) {
    console.log(error.response, error.response.body, 'a[pi send grif error');
    return;
  }

  if (errorUser) {
    console.log(
      errorUser.response,
      errorUser.response.body,
      'a[pi send grif errorUser',
    );
    return;
  }
}
