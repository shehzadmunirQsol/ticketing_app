import React, { useEffect, useState } from 'react';
import { useSelector } from 'react-redux';
import ProductCard from '~/components/common/card';
import ProductListCard from '~/components/common/cardlistview';
import Glow from '~/components/common/glow';
import { RootState } from '~/store/store';
import { trpc } from '~/utils/trpc';
import FeaturedCars from './featured_cars';
import langContent from '~/locales';
import ProductSection from '../home/product_section';

const CarsPage = () => {
  const { lang } = useSelector((state: RootState) => state.layout);

  const [cardView, setCardView] = useState<any>('cardview');

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

  const { data: prductsList } = trpc.event.getByCategoryId.useQuery(filters, {
    refetchOnWindowFocus: false,
    onSuccess(data) {
      if (filters.first > 0 && data?.data?.length) {
        setProducts([...products, ...data.data]);
      } else if (data?.data?.length) {
        setProducts(data.data);
      }
    },
  });

  function nextPage() {
    if (
      prductsList?.data?.length === filters.rows &&
      prductsList?.count > products.length
    ) {
      setFilters({ ...filters, first: ++filters.first });
    }
  }
  

  return (
    <div className="mx-auto  w-full bg-background">
      {/* this div below ↓ it to add spacing to avoid header */}
      <div className="relative pt-24"></div>
      <FeaturedCars />
      <div className="h-full  space-y-8 py-8 md:py-14 px-4 md:px-14 md:space-y-12 md:py-20">
        <div className="relative">
          <p className="hidden slg:block pb-6 text-2xl md:text-5xl tracking-tighter font-extrabold text-white ">
            {langContent[lang.lang].Cars.HEADING}
          </p>

          <div className="block slg:hidden">
            <div className="flex justify-end listtabs mb-4">
              <span className={`tabbtn ${cardView === 'cardview' ? 'active' : ''}`} onClick={() => setCardView('cardview')}><i className="fa-regular fa-square-full"></i></span>
              <span className={`tabbtn ml-3 ${cardView === 'listview' ? 'active' : ''}`} onClick={() => setCardView('listview')}><i className="fa-solid fa-align-justify"></i></span>
            </div>
          </div>

          <Glow className="absolute  top-1/2 -left-16 w-1/5 h-[350px] overflow-hidden " />
          <Glow className="absolute  bottom-0 -right-16 w-1/5 h-[350px] overflow-hidden " />


          {
            cardView === "cardview" ?
              <div className="grid gap-8 md:gap-6 grid-cols-1 sm:grid-cols-2 z-40 lg:grid-cols-3 justify-between mx-auto ">
                {/* {products?.map((itemList, i) => {
                  return (
                    <div className="z-40" key={itemList?.id}>
                      <ProductCard
                        isLast={i === products.length - 1}
                        nextPage={nextPage}
                        dir={lang.dir}
                        data={itemList}
                        class="z-50"
                      />
                    </div>
                  );
                })} */}

                {products
                  ?.sort((a, b) => {
                    const isLastDayA =
                      a?.draw_date === null && a?.end_date
                        ? a?.end_date?.getTime() <= Date.now() + 24 * 60 * 60 * 1000
                        : false;

                    const isLastDayB =
                      b?.draw_date === null && b?.end_date
                        ? b?.end_date?.getTime() <= Date.now() + 24 * 60 * 60 * 1000
                        : false;
                    return isLastDayB ? 1 : isLastDayA ? -1 : 0;
                  })
                  .map((itemList, i) => (
                    <div className="z-40" key={itemList?.id}>
                      <ProductCard
                        isLast={i === products.length - 1}
                        nextPage={nextPage}
                        dir={lang.dir}
                        data={itemList}
                        class="z-50"
                      />
                    </div>
                  ))}
              </div>
              :
              <div className="listviewbx grid gap-5 md:gap-6 grid-cols-1 sm:grid-cols-2 z-40 justify-between mx-auto">
                {products?.map((itemList, i) => {
                  return (
                    <div className="z-40" key={itemList?.id}>
                      <ProductListCard
                        isLast={i === products.length - 1}
                        nextPage={nextPage}
                        dir={lang.dir}
                        data={itemList}
                        class="z-50"
                      />
                    </div>
                  );
                })}
              </div>
          }


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

        <ProductSection
          class="w-3/5 md:w-full"
          slidesToShow={3}
          center={false}
          title={'Closed Competitions'}
          type="drawn"
          breakpoint={[3, 2, 1.5]}
          breakpointScreens={[1350, 1050, 800]}
          categoryId={1}
        />
      </div>

      {/* <Testimonials /> */}
    </div>
  );
};

export default CarsPage;
