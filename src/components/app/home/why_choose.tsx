import React from 'react';
import Frame11 from '~/public/assets/icons/Frame16.svg';
import Frame12 from '~/public/assets/icons/Group14.png';
import Frame13 from '~/public/assets/icons/Frame15.svg';
import langContent from '~/locales';
import { useSelector } from 'react-redux';
import { RootState } from '~/store/store';
import NextImage from '~/components/ui/img';

function WhyChoose() {
  const { lang } = useSelector((state: RootState) => state.layout);

  return (
    <div className="relative w-full mx-auto mb-2 px-4 md:px-14 sm:pt-8">
      <div className="text-gray-200 text-center text-3xl  ltr:sm:text-left rtl:sm:text-right  sm:text-5xl font-black uppercase ">
        {langContent[lang.lang].Index.choose.HEADING}
      </div>

      <div className="grid grid-cols-1 gap-8 py-8 md:grid-cols-3">
        {langContent[lang.lang].Index.choose.array?.map((item, index) => {
          return (
            <div
              key={index}
              className="text-center py-3 sm:py-6 w-full space-y-2 md:space-y-3 text-white items-center border-t-2  md:border-b-2 md:border-b-border  border-t-primary "
            >
              <div className="text-center h-20 mx-auto w-20">
                <NextImage
                  className="w-full h-full object-contain "
                  src={images[item.img]}
                  quality={100}
                  alt="Sunset in the mountains"
                />
              </div>
              <div>
                <div className="text-center">
                  <div className="text-2xl sm:text-3xl leading-tight text-center">
                    {item?.title}
                  </div>
                  <div className="text-base font-normal text-gray-500 text-border leading-tight text-center mt-2">
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

const images: any = {
  1: Frame11,
  2: Frame12,
  3: Frame13,
};
