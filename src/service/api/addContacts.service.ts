export type AddContactPayloadType = {
  email: string;
  attributes: {
    lastname?: string;
    firstname?: string;
    FULL_NAME?: string;
    PHONE?: string;
    DATE_OF_BIRTH?: string;
    GENDER?: '1' | '2';
    COUNTRY?: string;
    STATE?: string;
    CITY?: string;
    ADDRESS?: string;
  };
};

export async function addContactsToBrevoList(payload: AddContactPayloadType) {
  try {
    const options = {
      method: 'POST',
      headers: {
        accept: 'application/json',
        'content-type': 'application/json',
        'api-key': process.env.BREVO_EMAIL_API_KEY as string,
      },
      body: JSON.stringify({
        attributes: payload.attributes,
        updateEnabled: false,
        email: payload.email,
        listIds: [17],
      }),
    };

    await fetch('https://api.brevo.com/v3/contacts', options);
    return { success: true, message: 'Contact added successfully!' };
  } catch (error: any) {
    console.log(error, 'api.brevo.com error');
  }
}
