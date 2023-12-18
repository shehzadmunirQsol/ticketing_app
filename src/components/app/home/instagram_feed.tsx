import React from 'react';
import { ElfsightWidget } from 'react-elfsight-widget';

interface producctInterface {
  title: string;
  subTitle: string;
}

function InstagramFeed(props: producctInterface) {

  return (

    <div className="instapost px-4 md:px-14 py-6 md:py-12">
    <div className="relative pb-2 md:pb-8 w-full">
      <div>
        <p className="text-gray-200 text-left sm:text-center text-3xl ltr:sm:text-left rtl:sm:text-right  sm:text-5xl font-black uppercase">
          {props?.title}
        </p>
        <p className="text-gray-200 !text-xl sm:!text-3xl text-left sm:text-center ltr:sm:text-left rtl:sm:text-right lg:!text-5xl font-light uppercase  ">
          {props?.subTitle}
        </p>
      </div>
    </div>
    <ElfsightWidget widgetId="bd69b607-c562-42c6-9b4a-8ce9b3b410b5" />
    </div>

  );
}

export default InstagramFeed;
