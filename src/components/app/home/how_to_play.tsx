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
      title: 'Answer the question correctly',
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
      <div className="relative top-0  max-w-[1600px] mx-auto    grid grid-cols-1 lg:grid-cols-2  p-4 md:p-14 ">
        <div className=" my-auto   w-full ">
          <p className="mr-auto  py-4 md:py-0 text-left !w-full md:!max-w-[460px] text-black font-[1000] tracking-[-4px] !text-6xl   md:!text-6xl lg:!text-8xl   uppercase">
            HOW TO PLAY?
          </p>
        </div>

        <div className="flex flex-col gap-8 space-y-4 ">
          {HowtoStart?.map((item, index) => {
            return (
              <div key={index} className="flex gap-4  items-center">
                <div className=" h-12 w-12">
                  <Image
                    className="w-full h-full object-cover "
                    src={item?.icon}
                    height={400}
                    quality={100}
                    alt="Sunset in the mountains"
                  />
                </div>
                <div>
                  <div>
                    <div className="text-background text-sm font-normal leading-tight">
                      0{item?.step} STEP
                    </div>
                    <div className="text-background text-lg font-extrabold leading-2 ">
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
