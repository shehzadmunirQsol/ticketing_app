import React from 'react';
import image1 from '../../../public/assets/only.svg';
import image2 from '../../../public/assets/entires.svg';
import image3 from '../../../public/assets/maxperson.svg';
import image4 from '../../../public/assets/wordwide.svg';
import Image from 'next/image';
import langContent from '~/locales';
import { useSelector } from 'react-redux';
import { RootState } from '~/store/store';

const EntiresDetail = (props: any) => {
  const { lang } = useSelector((state: RootState) => state.layout);

  const dynamicData: any = {
    1: `AED ${props?.data?.price ?? 0?.toFixed(0)?.toLocaleString()}`,
    2: props?.data?.total_tickets?.toLocaleString() ?? 0,
  };

  return (
    <div className="my-12 bg-backgroundDark  rounded-md border  border-border ">
      <div className="container px-10 py-10 w-full">
        <div className="flex flex-col -m-4 text-center justify-center items-start gap-4 md:gap-8 lg:items-center lg:justify-between lg:gap-0  lg:flex-row">
          {langContent[lang.lang].ProductDetail.entries.array.map(
            (item, index) => {
              return (
                <div
                  key={index}
                  className={`flex ${
                    item.LINK
                      ? 'flex-col justify-start '
                      : 'flex-row justify-center'
                  } items-center `}
                >
                  <div className={`flex  justify-center items-center `}>
                    <Image
                      src={images[item.data]}
                      className="ltr:mr-4 rtl:ml-4"
                      alt={'image'}
                    />
                    <p className="leading-relaxed  text-lg md:text-xl text-primary font-semibold">
                      {item.HEADING}{' '}
                      {item.data === 3 ? (
                        <span className="font-black">
                          {props?.data?.user_ticket_limit}
                        </span>
                      ) : (
                        ''
                      )}{' '}
                      {item.SUB_HEADING}
                      {!item.LINK && item.data !== 3 ? (
                        <span className="font-black ml-2">
                          {dynamicData[item.data]}
                        </span>
                      ) : (
                        ''
                      )}
                    </p>
                  </div>
                  {item.LINK && (
                    <p className="text-white text-sm underline -mt-2 -ml-5">
                      {item?.LINK}
                    </p>
                  )}
                </div>
              );
            },
          )}
        </div>
      </div>
    </div>
  );
};

export default EntiresDetail;

const images: any = {
  1: image1,
  2: image2,
  3: image3,
  4: image4,
};
