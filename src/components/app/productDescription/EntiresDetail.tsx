import React from 'react';
import image1 from '../../../public/assets/only.svg';
import image2 from '../../../public/assets/entires.svg';
import image3 from '../../../public/assets/maxperson.svg';
import image4 from '../../../public/assets/wordwide.svg';
import langContent from '~/locales';
import { useSelector } from 'react-redux';
import { RootState } from '~/store/store';
import NextImage from '~/components/ui/img';

const EntiresDetail = (props: any) => {
  const { lang } = useSelector((state: RootState) => state.layout);

  const dynamicData: any = {
    1: `AED ${props?.data?.price ?? 0?.toFixed(0)?.toLocaleString()}`,
    2: props?.data?.total_tickets?.toLocaleString() ?? 0,
  };

  return (
    <div className="my-6 sm:my-12 bg-backgroundDark  rounded-md border  border-border ">
      <div className="p-4 sm:p-6 w-full">
        <div className="grid grid-cols-2 lg:grid-cols-4 gap-4">
          {langContent[lang.lang].ProductDetail.entries.array.map(
            (item, index) => {
              return (
                <div
                  key={index}
                  className={`flex ${
                    item.LINK
                      ? 'flex-row justify-start lg:justify-center items-center'
                      : 'flex-row justify-start lg:justify-center items-center'
                  } `}
                >
                  <div className={`flex  justify-center items-center `}>
                    <NextImage
                      src={images[item.data]}
                      className="ltr:mr-2 ltr:md:mr-3 rtl:ml-2 rtl:md:ml-3"
                      alt={'image'}
                    />
                    <p className="text-sm md:text-xl text-primary font-semibold">
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
                  {/* {item.LINK && (
                    <p className="text-white text-sm underline mt-0 ml-5 self-end">
                      {item?.LINK}
                    </p>
                  )} */}
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
