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
  const eventFaqs = `
  <nav class="accordion w-full   arrows">
  <div class="mt-4">
  
      <input type="radio" name="accordion" id="cb1" />
      <section class="box border-b border-t   border-lightColorBorder hover:border-b-primary shadow-lg hover:shadow-2xl  pr-4 overflow-hidden">
        <label class="box-title  lg:text-3xl py-4 text-xl  font-bold text-white w-full flex items-center justify-between h-full" for="cb1"><p>How is the winner chosen?</p>  <i class="fas fa-chevron-down icon-class"></i></label>
        <label class="box-close" for="acc-close"></label>
        <div class="box-content text-base mt-4 text-grayColor pb-4">Your ticket number(s) will be shown as soon as your order is confirmed and will be available under <span class="text-primary font-bold">&lsquo;My Account&rsquo;</span> and in your email confirmation.</div>
      </section>
      <input type="radio" name="accordion" id="cb2" />
      <section class="box border-b border-lightColorBorder hover:border-b-primary shadow-lg hover:shadow-2xl pr-4 overflow-hidden">
        <label class="box-title  py-4 lg:text-3xl text-xl font-bold text-white w-full flex items-center justify-between " for="cb2"><p>What if a competition does not sell out?</p>  <i class="fas fa-chevron-down icon-class"></i></label>
        <label class="box-close" for="acc-close"></label>
        <div class="box-content text-base mt-4 text-grayColor pb-4">Your ticket number(s) will be shown as soon as your order is confirmed and will be available under <span class="text-primary font-bold">&lsquo;My Account&rsquo;</span> and in your email confirmation.</div>
      </section>
      <input type="radio" name="accordion" id="cb3" />
      <section class="box border-b border-lightColorBorder hover:border-b-primary shadow-lg hover:shadow-2xl  pr-4 overflow-hidden">
        <label class="box-title  py-4 lg:text-3xl text-xl font-bold text-white w-full flex items-center justify-between " for="cb3"><p>Is ‘Winnar’ a scam?</p>  <i class="fas fa-chevron-down icon-class"></i></label>
        <label class="box-close" for="acc-close"></label>
        <div class="box-content text-base mt-4 text-grayColor pb-4">Your ticket number(s) will be shown as soon as your order is confirmed and will be available under <span class="text-primary font-bold">&lsquo;My Account&rsquo;</span> and in your email confirmation.</div>
      </section>
      <input type="radio" name="accordion" id="cb4" />
      <section class="box border-b  border-lightColorBorder hover:border-b-primary shadow-lg hover:shadow-2xl  pr-4 overflow-hidden">
        <label class="box-title  py-4 lg:text-3xl text-xl font-bold text-white w-full flex items-center justify-between " for="cb4"><p>Why am I receiving communications from Winnar?</p>  <i class="fas fa-chevron-down icon-class"></i></label>
        <label class="box-close" for="acc-close"></label>
        <div class="box-content text-base mt-4 text-grayColor pb-4">Your ticket number(s) will be shown as soon as your order is confirmed and will be available under <span class="text-primary font-bold">&lsquo;My Account&rsquo;</span> and in your email confirmation.</div>
      </section>
      <input type="radio" name="accordion" id="acc-close" />
      </div>
    </nav>
  `
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
  const reactElementsEnglish = parse(eventFaqs || '', {
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
          {slug === 'about-us' || slug === 'faq' ? (
            <div>{reactElementsEnglish}</div>
          ) : (
            <div
            className="min-h-screen mt-10 cmsStyle px-4 md:px-14"
              dangerouslySetInnerHTML={
                {
                  __html: data?.en?.toString() ?? 'HTML CONTENT NOT FOUND',
                }
              }
            />
          )}
        </TabsContent>
        <TabsContent value="ar">
        {slug === 'about-us' || slug === 'faq' ? (
            <div dir="rtl">{reactElementsArabic}</div>
          ) : (
            <div
            className="min-h-screen mt-10 cmsStyle px-4 md:px-14"
              dangerouslySetInnerHTML={
                {
                  __html: data?.ar?.toString() ?? 'HTML CONTENT NOT FOUND',
                } as any
              }
            />
          )}
        </TabsContent>
      </Tabs>
    </div>
  );
};

export default Preview;
