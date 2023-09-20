import React, { useRef } from 'react';
import Slider from 'react-slick';

import 'slick-carousel/slick/slick.css';
import 'slick-carousel/slick/slick-theme.css';

import Frame11 from '~/public/assets/icons/Frame16.svg';
import Frame12 from '~/public/assets/icons/Group14.png';
import Frame13 from '~/public/assets/icons/Frame15.svg';

import Image from 'next/image';
interface producctInterface {
  class?: string;
  title: string;
  center: boolean;
  slidesToShow?: number;
}
function WhyChoose() {
  const HowtoStart = [
    {
      step: 1,
      title: 'Great Odds',
      desc: 'Limited tickets in each draw',
      icon: Frame11,
    },
    {
      step: 2,
      title: 'Transparent',
      desc: 'We show the number of entries in every draw',
      icon: Frame12,
    },
    {
      step: 3,
      title: 'Guaranteed',
      desc: "Guaranteed winner even if we don't sell out",
      icon: Frame13,
    },
  ];
  return (
    <div className="relative   w-full max-w-[1600px] mx-auto mb-2 px-4 md:px-14 py-8">
      <div className="text-gray-200 text-5xl font-black uppercase ">
        WHY CHOOSE US?
      </div>

      <div className="grid grid-cols-1 md:grid-cols-3 gap-8   py-8 ">
        {HowtoStart?.map((item, index) => {
          return (
            <div
              key={index}
              className=" py-6 max-w-2xl space-y-4 text-white items-center border-t-2  md:border-b-2 md:border-b-border  border-t-primary "
            >
              <div className=" h-20 w-20">
                <Image
                  className="w-full h-full object-contain "
                  src={item?.icon}
                  quality={100}
                  alt="Sunset in the mountains"
                />
              </div>
              <div>
                <div>
                  <div className=" text-4xl  leading-tight">{item?.title}</div>
                  <div className=" text-base font-normal text-gray-500 text-border leading-tight">
                    {item?.desc}
                  </div>
                </div>
              </div>
            </div>
          );
        })}
      </div>
    </div>
  );
}

export default WhyChoose;
