import React from 'react';
import BannerTitle from '~/components/common/banner_title';
import CashBg from '~/public/assets/cash_bg.png';
import Cash from '~/public/assets/cash-1.png';
import { useSelector } from 'react-redux';
import { RootState } from '~/store/store';
import ProductCard from '~/components/common/card';
import Glow from '~/components/common/glow';

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

  const 
  return (
    <>
      <BannerTitle image={CashBg} text={'Cash'} />
      <div className="h-full  p-10">
        <Glow className=" absolute  top-50 -right-16  p-2   w-1/5 h-[350px]  " />
        <Glow className=" absolute  bottom-96 -right-16  w-1/5 h-[350px] " />
        <div className=" grid grid-cols-1 md:grid-cols-2     lg:grid-cols-3   justify-between max-w-[1300px] mx-auto ">
          {cashList.map((item, i) => (
            <>
              <div className="mx-auto py-2 md:py-0">
                <ProductCard
                  dir={lang.dir}
                  cash={item}
                  class="z-50 h-full max-w-sm lg:max-w-2xl md:scale-95  w-full  "
                />
              </div>
            </>
          ))}
        </div>
      </div>
    </>
  );
};

export default CashPage;
