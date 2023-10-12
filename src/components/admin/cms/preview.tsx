import Head from 'next/head.js';
import { useRouter } from 'next/router';
import Script from 'next/script';
import React, { useEffect, useState } from 'react';
import { Tabs, TabsContent, TabsList, TabsTrigger } from '~/components/ui/tabs';
import parse from 'html-react-parser';
import AboutCarousel from '~/components/app/about/about_carousel';
import { Lang } from '~/store/reducers/layout';

const Preview = () => {
  const router = useRouter();
  const [data, setData] = useState<any>({});
  const [slug, setSlug] = useState<any>('');

  const en: Lang = {
    dir: 'ltr',
    lang: 'en',
    lang_id: 1,
  };

  const ar: Lang = {
    dir: 'rtl',
    lang: 'ar',
    lang_id: 2,
  };

  // Function to read data from localStorage
  const getDataFromLocalStorage = () => {
    const storedData = JSON.parse(localStorage.getItem('cmscontent') as string);
    const slugData = JSON.parse(localStorage.getItem('cmsslug') as string);
    console.log(slugData, 'slugData');
    if (storedData) {
      setData(storedData);
    }
    if (slugData) {
      setSlug(slugData);
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

  let carouselHeading: any = {};
  const findElementsWithAttribute = (node: any, lang: Lang) => {
    if (node.type === 'tag') {
      const shortcode = node?.attribs;
      if (shortcode?.data === 'main-carousel-heading') {
        carouselHeading = JSON.parse(node?.children[0]?.data) as any;
        return <></>;
      }

      if (shortcode?.data === 'main-carousel') {
        const imageCrousel = JSON.parse(node?.children[0]?.data) as [];
        if (imageCrousel.length) {
          return (
            <AboutCarousel
              imageCrousel={imageCrousel}
              heading={carouselHeading}
              lang={lang}
            />
          );
        }
      } else {
        return node;
      }
    }
  };
  const reactElementsEnglish = parse(data?.en?.toString() || '', {
    replace: (node: any) => findElementsWithAttribute(node, en),
  });
  const reactElementsArabic = parse(data?.ar?.toString() || '', {
    replace: (node: any) => findElementsWithAttribute(node, ar),
  });
  console.log(data?.en?.toString(), 'data?.en?.toString()');

  return (
    <div className="space-y-8 ">
      <Tabs defaultValue={'en'} className="w-full">
        <TabsList className="overflow-hidden mt-5">
          <TabsTrigger value="en">English</TabsTrigger>
          <TabsTrigger value="ar">Arabic</TabsTrigger>
        </TabsList>
        <TabsContent value="en">
          <div className="min-h-screen mt-10 cmsStyle px-4 md:px-14">{reactElementsEnglish}</div>
        </TabsContent>
        <TabsContent value="ar">
            <div dir="rtl" className="min-h-screen mt-10 cmsStyle px-4 md:px-14">{reactElementsArabic}</div>
        </TabsContent>
      </Tabs>
    </div>
  );
};

export default Preview;
