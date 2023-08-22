import React from 'react';
import BannerTitle from '~/components/common/banner_title';
import CashBg from '~/public/assets/cash_bg.png';
import Cash from '~/public/assets/cash-1.png';
import { useSelector } from 'react-redux';
import { RootState } from '~/store/store';
import ProductCard from '~/components/common/card';

const CashPage = () => {
  const { lang } = useSelector((state: RootState) => state.layout);
  const cashList = [
    Cash,
    Cash,
    Cash,
    Cash,
    Cash,
    Cash,
    Cash,
    Cash,
    Cash,
    Cash,
    Cash,
    Cash,
    Cash,
    Cash,
    Cash,
    Cash,
  ];

  return (
    <>
      <BannerTitle image={CashBg} text={'Cash'} />
      <div className="h-full max-w-[1600px] mx-auto py-10">
        <div className=" grid grid-cols-1 md:grid-cols-2 mx-4   z-50  lg:grid-cols-3 xl:grid-cols-4  justify-between ">
          {cashList.map((item) => (
            <div className="">
              <ProductCard
                dir={lang.dir}
                cash={item}
                class="z-50 h-full lg:scale-90 md:scale-75 scale-50"
              />
            </div>
          ))}
        </div>
      </div>
    </>
  );
};

export default CashPage;
