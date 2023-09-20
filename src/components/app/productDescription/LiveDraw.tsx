import React, { useRef } from 'react';
import Slider from 'react-slick';

import 'slick-carousel/slick/slick.css';
import 'slick-carousel/slick/slick-theme.css';

import Check from '~/public/assets/check.svg';
import calender from '~/public/assets/calender.svg';
import clock from '~/public/assets/watch.svg';

import Image from 'next/image';
import { displayDate } from '~/utils/helper';
function LiveDraw(props: any) {
  const LiveDraw = [
    {
      title: 'Draw takes place regardless of sell out',
      icon: Check,
    },
    {
      title:
        'Competition will close sooner if the maximum entries are received ',
      icon: Check,
    },
    {
      title: 'Interactive live draw on our Facebook and Youtube page',
      icon: Check,
    },
  ];
  return (
    <div className="relative max-w-[1600px] mx-auto w-full mb-20">
      <div className="   bg-primary py-10  grid grid-cols-1 lg:grid-cols-2 items-start  ">
        <div className="   flex flex-col justify-center items-center  w-full ">
          <p className="  py-4 md:py-0 text-left !w-full md:!max-w-[310px] sm:!max-w-[310px] xs:!max-w-[310px] md:mb-30 text-black font-[1000] tracking-[-4px] !text-6xl   md:!text-6xl lg:!text-8xl uppercase">
            LIVE DRAW
          </p>
          <div className=" flex justify-start items-start border-2 border-gray-900 border-l-0 border-r-0   py-4 m-auto mx-10 md:py-0 text-left !w-full md:!max-w-[310px] sm:!max-w-[310px] xs:!max-w-[310px] mt-10  ">
            <div className="flex justify-start items-center my-2 border-2 border-t-0 border-l-0 border-b-0 border-gray-900 w-30   pr-4">
              <Image
                width={3}
                height={3}
                className="w-10 h-10 object-cover"
                src={calender.src}
                alt="Sunset in the mountains"
              />{' '}
              <p className="text-md font-semibold text-black">
                {props?.data?.end_date?.toISOString().split('T')[0]}
              </p>
            </div>
            <div className="flex justify-center items-center ml-4 my-2">
              <Image
                width={3}
                height={3}
                className="w-10 h-10 object-cover"
                src={clock.src}
                alt="Sunset in the mountains"
              />{' '}
              <p className="text-md font-semibold text-black">
                {props?.data?.end_date?.getHours()}:
                {props?.data?.end_date?.getMinutes()} PM GST
              </p>
            </div>
          </div>
        </div>

        <div className="flex flex-col gap-8 space-y-4   justify-center my-auto max-w-lg text-left ">
          {LiveDraw?.map((item, index) => {
            return (
              <div
                key={index}
                className="flex gap-4  items-center md:pl-4 sm:pl-4  xs:pl-4  "
              >
                <Image
                  className="w-6 h-6 object-cover"
                  src={item?.icon}
                  alt="Sunset in the mountains"
                />
                <p className="text-background text-xl font-medium ">
                  {item?.title}
                </p>
              </div>
            );
          })}
        </div>
      </div>
    </div>
  );
}

export default LiveDraw;
