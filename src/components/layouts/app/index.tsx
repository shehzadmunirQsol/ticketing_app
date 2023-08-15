import React, { ReactNode } from 'react';
import { Button } from '~/components/ui/button';
import Header from './header';

type DefaultLayoutProps = { children: ReactNode };

function index({ children }: DefaultLayoutProps) {
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
      <div className="relative w-full overflow-x-hidden">
        
          <Header />
        <div className="w-full">{children}</div>
      </div>
      <Button onClick={handleClick}>Test Email</Button>
    </>
  );
}

export default index;
