import React from 'react';
import { Button } from '~/components/ui/button';

function index() {
  const handleClick = async () => {
    const res = await fetch(
      `${process.env.NEXT_PUBLIC_BASE_URL}/api/email/mailer`,
      {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({
          email: 'hassanshanqsols@gmail.com',
        }),
      },
    );

    const { error } = await res.json();
    if (error) {
      console.log(error.response, error.response.body, 'api brevo error');
      return;
    }
  };

  return (
    <>
      <header>My Header</header>
      <div className="flex">
        Sidebar
        <div className="flex-1">Main content</div>
      </div>
      <Button onClick={handleClick}>Test Email</Button>
    </>
  );
}

export default index;
