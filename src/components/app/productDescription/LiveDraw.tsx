import React from 'react';
import Icon from '~/public/assets/check.svg';
import calender from '~/public/assets/calender.svg';
import clock from '~/public/assets/watch.svg';
import langContent from '~/locales';

import Image from 'next/image';
import { useSelector } from 'react-redux';
import { RootState } from '~/store/store';

function LiveDraw(props: any) {

  const { lang } = useSelector((state: RootState) => state.layout);

  return (
    <div className="relative  mb-20 max-w-[1600px]  ">
      <div className="   bg-primary lg:py-10  grid grid-cols-1 lg:grid-cols-2 gap-4 items-center md:items-start  px-4 md:px-14">
        <div className="   flex flex-col justify-center items-center  w-full ">
          <p className="  py-4 md:py-0 ltr:text-left rtl:text-right    !w-full md:!max-w-[310px] sm:!max-w-[310px] xs:!max-w-[310px] md:mb-30 text-background font-[1000] tracking-[-3px] !text-6xl   md:!text-6xl lg:!text-8xl uppercase">
            {langContent[lang.lang].ProductDetail.livedraw.HEADING}
          </p>
          <div className=" flex justify-start items-start border-2 border-gray-900 border-l-0 border-r-0   py-4 m-auto mx-10 md:py-0 text-left !w-full md:!max-w-[310px] sm:!max-w-[310px] xs:!max-w-[310px] lg:mt-10  ">
            <div className={`flex justify-start items-center my-2 border-2 border-t-0 ${lang.lang === "en" ? "border-l-0 pr-4 " : "border-r-0 pl-4" }  border-b-0 border-gray-900 w-30 `}>
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

        <div className="flex flex-col gap-8 lg:space-y-4 justify-center my-auto max-w-lg text-left mb-4">
          {langContent[lang.lang].ProductDetail.livedraw.array?.map((item, index) => {
            return (
              <div
                key={index}
                className="flex gap-4  items-center md:pl-4 sm:pl-4  xs:pl-4  "
              >
                <Image
                  className="w-6 h-6 object-cover"
                  src={Icon}
                  alt="Sunset in the mountains"
                />
                <p className="text-background text-xl font-medium  ltr:text-left rtl:text-right " >
                  {item?.description}
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
