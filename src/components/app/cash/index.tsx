'use client';
import React, { useEffect, useState } from 'react';
import BannerTitle from '~/components/common/banner_title';
import CashBg from '~/public/assets/cash_bg.png';
import Cash from '~/public/assets/cash-1.png';
import { useSelector } from 'react-redux';
import { RootState } from '~/store/store';
import ProductCard from '~/components/common/card';
import Glow from '~/components/common/glow';
import { trpc } from '~/utils/trpc';
import { LoadingDialog } from '~/components/common/modal/loadingModal';

const CashPage = () => {
  const { lang } = useSelector((state: RootState) => state.layout);
  const [products, setProducts] = useState<Array<any>>([]);
  const eventFilters = {
    lang_id: lang?.lang_id,
    first: 0,
    rows: 9,
    category_id: 2,
  };
  const [filters, setFilters] = useState({
    ...eventFilters,
  });

  useEffect(() => {
    setFilters({ ...filters, lang_id: lang.lang_id, first: 0 });
    setProducts([]);
  }, [lang.lang_id]);

  const {
    data: prductsList,
    isFetched,
    isLoading,
    isError,
  } = trpc.event.getByCategoryId.useQuery(filters, {
    refetchOnWindowFocus: false,
  });

  useEffect(() => {
    if (filters.first > 0 && prductsList?.data?.length) {
      setProducts([...products, ...prductsList?.data]);
    } else if (prductsList?.data?.length) {
      setProducts(prductsList?.data);
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
      <div className="relative h-full px-4 md:px-14 py-20">
        <Glow className=" absolute  top-1/4 -right-16  p-2   w-1/6 h-[150px]  " />
        <Glow className=" absolute  bottom-14 -right-16  w-1/6 h-[150px] " />
        <div className=" grid gap-6 grid-cols-1 md:grid-cols-2  z-40  lg:grid-cols-3   justify-between ">
          {products?.map((itemList, i) => {
            return (
              <div className="z-40" key={i}>
                <ProductCard
                  isLast={i === products.length - 1}
                  nextPage={nextPage}
                  dir={lang.dir}
                  cash={Cash}
                  data={itemList}
                />
              </div>
            );
          })}
        </div>

        {/* doudt should it load more on action or automatically */}
        {products.length != prductsList?.count ? (
          <div className="w-fit mx-auto">
            <div className="text-center my-4">
              <p className="tracking-tight font-bold">Load More</p>
              <i className="fas fa-arrow-down  text-teal-400 text-5xl my-2  "></i>
            </div>
          </div>
        ) : (
          ''
        )}
      </div>
    </>
  );
};

export default CashPage;
