import React from 'react';
import Head from 'next/head';
 import logo_img from "~/public/assets/account.png"

const SeoHead = ({ ...props }: any) => {
  return (
    <Head>
      {props?.cononcial_url ?? (
        <link rel="canonical" href={`https://${props?.cononcial_url}`} />
      )}
      <title>
        {props?.isLanding
          ? props?.title
          : `${props?.title} | Winnar`}
      </title>
      <meta property="og:title" content={props?.title} key="title" />
      <meta name="description" content={`${props?.description} Store`} />
      <meta property="og:title" content={props?.title} />
      <meta
        property="og:site_name"
        content={`${props?.domain_name} Store`}
      ></meta>
      <meta name="og:description" content={props?.description}></meta>
      <meta
        property="og:image"
        content={`${process.env.NEXT_PUBLIC_CLOUD_FRONT_BASE_URL}/${props?.meta_image}`}
      ></meta>
      <meta property="og:image:height" content="1260" />
      <meta property="og:image:width" content="2400" />
      <meta property="og:title" content="Winnar" />
      <meta property="og:url" content={`${props?.domain_name}`}></meta>
      <meta property="og:type" content="website"></meta>
      <meta
        name="keywords"
        content={`${props?.domain_name},  lottery `}
      ></meta>
      <meta property="og:site_name" content="Winnar" />
      <meta name="twitter:card" content="summary_large_image" />
      <meta name="twitter:creator"  />
      <meta name="twitter:description" content={`${props?.description}`} />
      <meta name="twitter:title" content={`${props?.title} title`} />
      <meta
        name="twitter:image"
        content={`${process.env.NEXT_PUBLIC_CLOUD_FRONT_BASE_URL}/${props?.meta_image}`}
      />
      <meta name="robots" content="noindex,nofollow" />
      <link
        rel="icon"
        href={`${process.env.NEXT_PUBLIC_CLOUD_FRONT_BASE_URL}/${
          props?.favicon_image ? props?.favicon_image : logo_img.src
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
