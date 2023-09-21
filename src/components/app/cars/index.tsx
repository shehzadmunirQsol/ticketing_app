import React, { useEffect, useState } from 'react';
import { useSelector } from 'react-redux';
import ProductCard from '~/components/common/card';
import Glow from '~/components/common/glow';
import { RootState } from '~/store/store';
import { trpc } from '~/utils/trpc';

import CarsBg from '~/public/assets/cars-bg-2.png';
import BannerTitle from '~/components/common/banner_title';
import FeaturedCars from './featured_cars';
import Testimonials from '../home/testimonials';
import { LoadingDialog } from '~/components/common/modal/loadingModal';

const CarsPage = () => {
  const { lang } = useSelector((state: RootState) => state.layout);
  const [products, setProducts] = useState<Array<any>>([]);
  const eventFilters = {
    lang_id: lang?.lang_id,
    first: 0,
    rows: 9,
    category_id: 1,
  };
  const [filters, setFilters] = useState({
    ...eventFilters,
  });

  useEffect(() => {
    setFilters({ ...filters, lang_id: lang.lang_id, first: 0 });
    setProducts([]);
  }, [lang.lang_id]);

  const { data: prductsList, isLoading } = trpc.event.getByCategoryId.useQuery(
    filters,
    {
      refetchOnWindowFocus: false,
    },
  );

  useEffect(() => {
    if (filters.first > 0 && prductsList?.data?.length) {
      setProducts([...products, ...prductsList.data]);
    } else if (prductsList?.data?.length) {
      setProducts(prductsList.data);
    }
  }, [prductsList]);

  function nextPage() {
    console.log('Next page emitted');
    if (products.length % filters.rows === 0) {
      setFilters({ ...filters, first: ++filters.first });
    }
  }

  return (
    <div className="mx-auto max-w-[1600px] w-full bg-background">
      {/* this div below ↓ it to add spacing to avoid header */}
      <div className="relative pt-24"></div>
      <FeaturedCars />
      <div className="block slg:hidden">
        <BannerTitle image={CarsBg} text={'Cars'} />
      </div>
      <div className="relative h-full  px-10 pb-20 ">
        <p className="  text-2xl md:text-5xl sm:px-0 px-10 pt-10 sm:pt-10 pb-6     tracking-tighter font-extrabold text-white ">
          CARS COMPETITION
        </p>
        <Glow className=" absolute  top-1/2 -left-16     w-1/5 h-[350px] overflow-hidden " />

        {/* <Glow className=" absolute   bottom-96 -right-16  w-1/5 h-[350px] overflow-x-hidden" /> */}
        <div className=" grid grid-cols-1 md:grid-cols-2 z-40 gap-4 lg:grid-cols-3  justify-between max-w-[1300px] mx-auto ">
          {products?.map((itemList, i) => {
            return (
              <div className="mx-auto py-2 md:py-0 z-40" key={i}>
                <ProductCard
                  isLast={i === products.length - 1}
                  nextPage={nextPage}
                  dir={lang.dir}
                  data={itemList}
                  class="z-50 md:max-w-[700px] lg:max-w-[1000px]  "
                />
              </div>
            );
          })}
        </div>

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
      <Testimonials />
      <LoadingDialog open={isLoading} text={'Loading...'} />
    </div>
  );
};

export default CarsPage;
