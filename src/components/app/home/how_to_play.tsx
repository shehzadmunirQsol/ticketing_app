import React from 'react';
import Frame11 from '~/public/assets/icons/Frame11.svg';
import Frame12 from '~/public/assets/icons/Frame12.svg';
import Frame13 from '~/public/assets/icons/Frame13.svg';
import Image from 'next/image';
import Glow from '~/components/common/glow';
import langContent from '~/locales';
import { useSelector } from 'react-redux';
import { RootState } from '~/store/store';


function HowtoSection() {
  const { lang } = useSelector((state: RootState) => state.layout);
  return (
    <div className="w-full  mb-16 bg-primary relative">
      <div className="relative top-0 mx-auto grid grid-cols-1 lg:grid-cols-2 gap-4 px-5 pt-7 pb-8 md:p-14">
        <div className=" my-auto   w-full ">
          <p className={`w-full md:!max-w-[460px] mr-auto pb-4 md:py-0 ltr:text-left rtl:text-right text-black font-black tracking-[-1px] text-3xl xsm:text-6xl lg:text-8xl`}>
          {langContent[lang.lang].Index.howtoplay.HEADING}
          </p>
        </div>

        <div className="flex flex-col gap-8 sm:space-y-4 ">
          {langContent[lang.lang].Index.howtoplay.array?.map((item, index) => {
            return (
              <div key={index} className="relative flex gap-2 md:gap-4   items-center ">
                <div className={`relative h-10 w-10  sm:h-12 sm:w-12 rounded-md overflow-hidden`}>
                  <Image
                    className="w-full h-full  object-cover "
                    src={images[item.img]}
                    fill
                    quality={100}
                    alt="Sunset in the mountains"
                  />
                </div>

                <div className="w-72 sm:w-[34rem]">
                  <div>
                    <div className="text-background text-xs sm:text-sm font-normal ">
                      {item?.step} 
                    </div>
                    <div className="text-background text-sm sm:text-md md:text-lg font-bold sm:font-extrabold  ">
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


const images:any= {
  1:Frame11,
  2:Frame12,
  3:Frame13,
}