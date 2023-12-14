import React from 'react';
import Icon from '~/public/assets/check.svg';
import calender from '~/public/assets/calender.svg';
import clock from '~/public/assets/watch.svg';
import langContent from '~/locales';
import { useSelector } from 'react-redux';
import { RootState } from '~/store/store';
import NextImage from '~/components/ui/img';

function LiveDraw(props: any) {
  const { lang } = useSelector((state: RootState) => state.layout);

  return (
    <div className="relative  mb-10 max-w-[1600px]  ">
      <div className="bg-primary py-4 lg:py-8  grid grid-cols-1 lg:grid-cols-2 gap-4 items-center md:items-start  px-4 md:px-14">
        <div className="flex flex-col justify-center items-center  w-full">
          <p className="py-4 md:py-0 ltr:text-left rtl:text-right md:mb-30 text-background font-black tracking-[-3px] !text-6xl md:!text-6xl lg:!text-7xl uppercase">
            {langContent[lang.lang].ProductDetail.livedraw.HEADING}
          </p>
          <div className="flex justify-center md:justify-start w-full md:w-auto items-start border-2 border-gray-900 border-l-0 border-r-0 py-2 md:py-4 m-auto mx-10 md:py-0 text-left lg:mt-10  ">
            <div
              className={`flex justify-start items-center my-2 border-2 border-t-0 ${
                lang.lang === 'en' ? 'border-l-0 pr-4 ' : 'border-r-0 pl-4'
              }  border-b-0 border-gray-900 w-30 `}
            >
              <NextImage
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
              <NextImage
                width={3}
                height={3}
                className="w-10 h-10 object-cover"
                src={clock.src}
                alt="Sunset in the mountains"
              />{' '}
              <p className="text-md font-semibold text-black">
                {props?.data?.end_date?.getHours()?.toString()?.length > 1
                  ? props?.data?.end_date?.getHours()
                  : '0' + props?.data?.end_date?.getHours()}
                :
                {props?.data?.end_date?.getMinutes()?.toString()?.length > 1
                  ? props?.data?.end_date?.getMinutes()
                  : '0' + props?.data?.end_date?.getMinutes()}{' '}
                PM GST
              </p>
            </div>
          </div>
        </div>

        <div className="flex flex-col gap-4 md:gap-4 lg:space-y-4 justify-center my-auto max-w-lg text-left mb-4 mt-4 md:mt-0">
          {langContent[lang.lang].ProductDetail.livedraw.array?.map(
            (item, index) => {
              return (
                <div
                  key={index}
                  className="flex gap-4  items-center md:pl-4 sm:pl-4  xs:pl-4  "
                >
                  <NextImage
                    className="w-6 h-6 object-cover"
                    src={Icon}
                    alt="Sunset in the mountains"
                  />
                  <p className="text-background text-base md:text-xl font-medium  ltr:text-left rtl:text-right ">
                    {item?.description}
                  </p>
                </div>
              );
            },
          )}
        </div>
      </div>
    </div>
  );
}

export default LiveDraw;
