import { useRouter } from 'next/router';
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
      {/* <p>hello</p> */}
      <div
        dangerouslySetInnerHTML={{
          __html: data?.toString() ?? '<>html conte</>',
        }}
      ></div>
    </div>
  );
};

export default Preview;
