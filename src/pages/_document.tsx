import { Html, Head, Main, NextScript } from 'next/document';
import Script from 'next/script';
import React from 'react';

export default function Document() {
  return (
    <Html>
      <Head>
        <link
          rel="stylesheet"
          href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.3.0/css/all.min.css"
          integrity="sha512-SzlrxWUlpfuzQ+pcUCosxcglQRNAq/DZjVsC0lE40xsADsfeQoEypE+enwcOiGjk/bSuGGKHEyjSoQ1zVisanQ=="
          crossOrigin="anonymous"
          referrerPolicy="no-referrer"
        />

        <script
          id="brevo-tracking"
          dangerouslySetInnerHTML={{
            __html: `
            (function() { 
              window.sib = {
              equeue: [],
              client_key: "quglegff81xcea5agnk739dp"
              };
              /* OPTIONAL: email for identify request*/
              // window.sib.email_id = 'example@domain.com';
              window.sendinblue = {};
              for (var j = ['track', 'identify', 'trackLink', 'page'], i = 0; i < j.length; i++) {
              (function(k) {
              window.sendinblue[k] = function() {
              var arg = Array.prototype.slice.call(arguments);
              (window.sib[k] || function() {
              var t = {};
              t[k] = arg;
              window.sib.equeue.push(t);
              })(arg[0], arg[1], arg[2], arg[3]);
              };
              })(j[i]);
              }
              var n = document.createElement("script"),
              i = document.getElementsByTagName("script")[0];
              n.type = "text/javascript", n.id = "sendinblue-js", n.async = !0, n.src = "https://sibautomation.com/sa.js?key=" + window.sib.client_key, i.parentNode.insertBefore(n, i), window.sendinblue.page();
              })();`,
          }}
        />
      </Head>
      <body>
        <Main />
        <NextScript />
      </body>
    </Html>
  );
}
