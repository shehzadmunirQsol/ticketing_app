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

        <script
          id="google-gtm-tracking"
          dangerouslySetInnerHTML={{
            __html: `
            (function (w, d, s, l, i) {
              w[l] = w[l] || []; w[l].push({
                'gtm.start':
                cookie_time, event: 'gtm.js'
              }); var f = d.getElementsByTagName(s)[0],
                j = d.createElement(s), dl = l != 'dataLayer' ? '&l=' + l : ''; j.async = true; j.src =
                  'https://www.googletagmanager.com/gtm.js?id=' + i + dl; f.parentNode.insertBefore(j, f);
            })(window, document, 'script', 'dataLayer', 'GTM-P4FRMH7C');
          `,
          }}
        />

        <script
          id="facebook-pixel-tracking"
          dangerouslySetInnerHTML={{
            __html: `
            !function(f,b,e,v,n,t,s)
            {if(f.fbq)return;n=f.fbq=function(){n.callMethod?
            n.callMethod.apply(n,arguments):n.queue.push(arguments)};
            if(!f._fbq)f._fbq=n;n.push=n;n.loaded=!0;n.version='2.0';
            n.queue=[];t=b.createElement(e);t.async=!0;
            t.src=v;s=b.getElementsByTagName(e)[0];
            s.parentNode.insertBefore(t,s)}
            (window,document,'script', 'https://connect.facebook.net/en_US/fbevents.js');
            fbq('init', '1022539718743092'); 
            fbq('track', 'PageView');
          `,
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
