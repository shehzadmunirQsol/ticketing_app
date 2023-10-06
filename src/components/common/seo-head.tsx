import React from 'react';
import Head from 'next/head';
import logo_img from "~/public/assets/account.png"

const SeoHead = ({ ...props }: any) => {
  return (
    <Head>
      <meta charSet="utf-8"></meta>
      <meta name="viewport" content="width=device-width, initial-scale=1, user-scalable=0"></meta>
      <meta httpEquiv="X-UA-Compatible" content="ie=edge"></meta>
      <meta name="description"
        content="Winnar, the prestige car raffle company that offers car enthusiasts the thrilling opportunity to win their coveted dream car and many other prizes."></meta>
      <meta name="keywords" content="WINNAR"></meta>

      <meta property="og:title" content="WINNAR"></meta>
      <meta property="og:description"
        content="Winnar, the prestige car raffle company that offers car enthusiasts the thrilling opportunity to win their coveted dream car and many other prizes."></meta>
      <meta property="og:image" id="baseurl" content="https://winnar.com/common/images/og.jpg"></meta>
      <meta property="og:url" content="https://winnar.com/"></meta>

      <meta name="twitter:card" content="summary_large_image"></meta>
      <meta name="twitter:title" content="WINNAR"></meta>
      <meta name="twitter:description"
        content="Winnar, the prestige car raffle company that offers car enthusiasts the thrilling opportunity to win their coveted dream car and many other prizes."></meta>
      <meta name="twitter:image" content="https://winnar.com/common/images/og-twitter.jpg">
      </meta>
      <title>WINNAR</title>

      <link
        rel="icon"
        href={`${process.env.NEXT_PUBLIC_CLOUD_FRONT_BASE_URL}/${props?.favicon_image ? props?.favicon_image : logo_img.src
          }`}
      />
      <link
        rel="stylesheet"
        href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.3.0/css/all.min.css"
        integrity="sha512-SzlrxWUlpfuzQ+pcUCosxcglQRNAq/DZjVsC0lE40xsADsfeQoEypE+enwcOiGjk/bSuGGKHEyjSoQ1zVisanQ=="
        crossOrigin="anonymous"
        referrerPolicy="no-referrer"
      />
    </Head>
  );
};

// This gets called on every request

export default SeoHead;
