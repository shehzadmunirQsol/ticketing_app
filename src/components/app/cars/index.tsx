import React, { useEffect, useState } from 'react';
import { useSelector } from 'react-redux';
import ProductCard from '~/components/common/card';
import Glow from '~/components/common/glow';
import { RootState } from '~/store/store';
import { trpc } from '~/utils/trpc';

import CarsBg from '~/public/assets/cars-bg-2.png';
import BannerTitle from '~/components/common/banner_title';
import FeaturedCars from './featured_cars';

const CarsPage = () => {
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

  console.log(filters, 'filters');
  const {
    data: prductsList,
    isFetched,
    isLoading,
    isError,
  } = trpc.settings.get_banner.useQuery(filters, {
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

  console.log({ prductsList }, 'prductsList');
  console.log({ products }, 'products');
  return (
    <>
    {/* this div below ↓ it to add spacing to avoid header */}
    <div className='relative pt-24'></div>
    <FeaturedCars/>
      <div className="block sm:hidden">
        <BannerTitle image={CarsBg} text={'Cars'} />
      </div>
      <p className="  text-2xl md:text-5xl px-10 sm:px-16 pt-10 sm:pt-10 pb-6     tracking-tighter font-extrabold text-white ">
        CARS COMPETITION
      </p>
      <div className="h-full  px-10 pb-20 ">
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

export default CarsPage;
