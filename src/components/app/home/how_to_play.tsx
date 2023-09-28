import React, { useRef } from 'react';
import Slider from 'react-slick';

import 'slick-carousel/slick/slick.css';
import 'slick-carousel/slick/slick-theme.css';

import Frame11 from '~/public/assets/icons/Frame11.svg';
import Frame12 from '~/public/assets/icons/Frame12.svg';
import Frame13 from '~/public/assets/icons/Frame13.svg';

// import LinesBehind from "~/public/assets/Lines_Text_Big.png"
// import LinesBehind1 from "~/public/assets/Lines_Text_Big_2.png"

import Image from 'next/image';
import Glow from '~/components/common/glow';
interface producctInterface {
  class?: string;
  title: string;
  center: boolean;
  slidesToShow?: number;
}
function HowtoSection() {
  const HowtoStart = [
    {
      step: 1,
      title: 'Select your prize and entry',
      icon: Frame11,
    },
    {
      step: 2,
      title:
        'Purchase water bottle automatically donated to our charity partner',
      icon: Frame12,
    },
    {
      step: 3,
      title: 'Winner announced on Live Draw',
      icon: Frame13,
    },
  ];
  return (
    <div className="w-full  mb-16 bg-primary relative">
      <div className="relative top-0   mx-auto    grid grid-cols-1 lg:grid-cols-2 gap-4  p-4 md:p-14 ">
        <div className=" my-auto   w-full ">
          <p className="w-full md:!max-w-[460px] mr-auto  py-4 md:py-0 text-left text-black font-[1000] tracking-[-1px] text-3xl   xsm:text-6xl lg:text-8xl">
            HOW TO PLAY?
          </p>
        </div>

        <div className="flex flex-col gap-8 sm:space-y-4 ">
          {HowtoStart?.map((item, index) => {
            return (
              <div key={index} className="relative flex gap-2 md:gap-4   items-center ">
                
                <div className={`relative h-10 w-10  sm:h-12 sm:w-12 rounded-md overflow-hidden`}>
                  <Image
                    className="w-full h-full  object-cover "
                    src={item?.icon}
                    fill
                    quality={100}
                    alt="Sunset in the mountains"
                  />
                </div>

                <div className="w-72 sm:w-[34rem]">
                  <div>
                    <div className="text-background text-xs sm:text-sm font-normal ">
                      0{item?.step} STEP
                    </div>
                    <div className="text-background text-xs sm:text-md md:text-lg font-bold sm:font-extrabold  ">
                      {item?.title}
                    </div>
                  </div>
                </div>
              </div>
            );
          })}
        </div>
      </div>

      <Glow className="absolute   -bottom-32 right-0      w-1/5 h-[350px] overflow-hidden " />
    </div>
  );
}

export default HowtoSection;
