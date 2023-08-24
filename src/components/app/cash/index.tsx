'use client';
import React, { useEffect, useState } from 'react';
import BannerTitle from '~/components/common/banner_title';
import CashBg from '~/public/assets/cash_bg.png';
import { useSelector } from 'react-redux';
import { RootState } from '~/store/store';
import ProductCard from '~/components/common/card';
import Glow from '~/components/common/glow';
import { trpc } from '~/utils/trpc';

const CashPage = () => {
  const { lang } = useSelector((state: RootState) => state.layout);
  const [products, setProducts] = useState<Array<any>>([]);
  const [filters, setFilters] = useState({
    lang_id: lang.lang_id,
    page: 0,
    first: 0,
    rows: 4,
    is_enabled: true,
    group: 'WONDER',
  });

  const { data: prductsList } = trpc.settings.get_banner.useQuery(filters, {
    refetchOnWindowFocus: false,
  });

  useEffect(() => {
    if (filters.first > 0 && prductsList?.length) {
      setProducts([...products, ...prductsList]);
    } else if (prductsList?.length) {
      setProducts(prductsList);
    }
  }, [prductsList]);

  function nextPage() {
    console.log('Next page emitted');
    if (products.length % filters.rows === 0) {
      setFilters({ ...filters, first: 1 + filters.first });
    }
  }

  return (
    <>
      <div className="relative pt-24"></div>
      <BannerTitle image={CashBg} text={'Cash'} />
      <div className="h-full  px-10 py-20">
        <Glow className=" absolute  top-[560px] -right-16  p-2   w-1/5 h-[350px]  " />
        <Glow className=" absolute  bottom-96 -right-16  w-1/5 h-[350px] " />
        <div className=" grid grid-cols-1 md:grid-cols-2     lg:grid-cols-3   justify-between max-w-[1300px] mx-auto ">
          {products?.map((itemList, i) => {
            return (
              <div className="mx-auto py-2 md:py-0" key={i}>
                <ProductCard
                  isLast={i === products.length - 1}
                  nextPage={nextPage}
                  dir={lang.dir}
                  cash={itemList?.src}
                  class="z-50 h-full max-w-sm lg:max-w-2xl md:scale-95  w-full  "
                />
              </div>
            );
          })}
        </div>

        {/* {filters.first > 0 && isSuccess ? (
          ''
        ) : (
          <div className="w-fit mx-auto">
            <div className="text-center my-4">
              <p className="tracking-tight font-bold">Load More</p>
              <i className="fas fa-arrow-down  text-teal-400 text-5xl my-2  "></i>
            </div>
          </div>
        )} */}

        {/* doudt should it load more on action or automatically */}
        <div className="w-fit mx-auto">
          <div className="text-center my-4">
            <p className="tracking-tight font-bold">Load More</p>
            <i className="fas fa-arrow-down  text-teal-400 text-5xl my-2  "></i>
          </div>
        </div>
      </div>
    </>
  );
};

export default CashPage;
