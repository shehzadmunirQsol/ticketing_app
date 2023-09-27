import Head from 'next/head.js';
import { useRouter } from 'next/router';
import Script from 'next/script';
import React, { useEffect, useState } from 'react';
import { Tabs, TabsContent, TabsList, TabsTrigger } from '~/components/ui/tabs';

const Preview = () => {
  const router = useRouter();
  // const { data } = router.query;
  // console.log(data, 'HSJSGJSJHGUSJSHS');
  const [data, setData] = useState<any>({});

  // Function to read data from localStorage
  const getDataFromLocalStorage = () => {
    const storedData = JSON.parse(localStorage.getItem('cmscontent') as string);
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
    <div className="p-8 space-y-8 ">
      <Tabs defaultValue={'en'} className="w-full">
        <TabsList>
          <TabsTrigger value="en">English</TabsTrigger>
          <TabsTrigger value="ar">Arabic</TabsTrigger>
        </TabsList>
        <TabsContent value="en">
          <div
            dangerouslySetInnerHTML={{
              __html: data?.en?.toString() ?? '<>html conte</>',
            }}
          ></div>
        </TabsContent>
        <TabsContent value="ar">
          <div
            dir="rtl"
            dangerouslySetInnerHTML={{
              __html: data?.ar?.toString() ?? '<>html conte</>',
            }}
          ></div>
        </TabsContent>
      </Tabs>
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
</div>;

export default Preview;
