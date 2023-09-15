import Head from 'next/head.js';
import { useRouter } from 'next/router';
import Script from 'next/script';
import React, { useEffect, useState } from 'react';

const Preview = () => {
  const router = useRouter();
  // const { data } = router.query;
  // console.log(data, 'HSJSGJSJHGUSJSHS');
  const [data, setData] = useState('');

  // Function to read data from localStorage
  const getDataFromLocalStorage = () => {
    const storedData = localStorage.getItem('cmscontent');
    if (storedData) {
      setData(storedData);
    }
  };

  useEffect(() => {
    getDataFromLocalStorage();

    const handleRouteChange = () => {
      localStorage.removeItem('cmscontent');
    };

    router.events.on('routeChangeStart', handleRouteChange);

    return () => {
      router.events.off('routeChangeStart', handleRouteChange);
    };
  }, []);



  return (
    <div>
      <div
        dangerouslySetInnerHTML={{
          __html: data?.toString() ?? '<>html conte</>',
        }}
      ></div>
    </div>
  );
};


<div className="carousel">
  <div className="card">
    <img src="" alt="" />
    <h4></h4>
    <p></p>

    <div className="hover">
    <h4></h4>
    <p></p>

    </div>
  </div>
</div>

export default Preview;
