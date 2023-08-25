import React, { ReactNode } from 'react';
import { Button } from '~/components/ui/button';
import Header from './header';
import { useSelector } from 'react-redux';
import { RootState } from '~/store/store';
import Footer from './footer';
import { Toaster } from '~/components/ui/toaster';

type DefaultLayoutProps = { children: ReactNode };

function Index({ children }: DefaultLayoutProps) {
  const { lang } = useSelector((state: RootState) => state.layout);

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
    <div dir={lang.dir} lang={lang.lang}>
      <Header />
      {children}

      <Footer />
    </div>
  );
}

export default Index;
