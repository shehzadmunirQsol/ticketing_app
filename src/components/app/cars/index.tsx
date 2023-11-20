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
      <div className=" h-full px-4 space-y-8 py-14 md:px-14 md:space-y-12 md:py-20">
        <div className="relative">
          <p className="hidden slg:block pb-6 text-2xl md:text-5xl tracking-tighter font-extrabold text-white ">
            {langContent[lang.lang].Cars.HEADING}
          </p>

          {/* <div>
            <span onClick={() => setCardView('cardview')}>Card View</span>
            <span onClick={() => setCardView('listview')}>List View</span>
          </div> */}

          <Glow className="absolute  top-1/2 -left-16 w-1/5 h-[350px] overflow-hidden " />
          <Glow className="absolute  bottom-0 -right-16 w-1/5 h-[350px] overflow-hidden " />


          {
            cardView === "cardview" ?
              <div className="grid gap-8 md:gap-6 grid-cols-1 sm:grid-cols-2 z-40 lg:grid-cols-3 justify-between mx-auto ">
                {products?.map((itemList, i) => {
                  return (
                    <div className="z-40" key={itemList?.id}>
                      <ProductCard
                        isLast={i === products.length - 1}
                        nextPage={nextPage}
                        dir={lang.dir}
                        data={itemList}
                        class="z-50 "
                      />
                    </div>
                  );
                })}
              </div>
              :
              <div className="justify-between mx-auto">
                {products?.map((itemList, i) => {
                  return (
                    <div className="z-40" key={itemList?.id}>
                      <ProductListCard
                        isLast={i === products.length - 1}
                        nextPage={nextPage}
                        dir={lang.dir}
                        data={itemList}
                        class="z-50 "
                      />
                    </div>
                  );
                })}
              </div>
          }

          {/* <div className="grid gap-8 md:gap-6 grid-cols-1 sm:grid-cols-2 z-40 lg:grid-cols-3 justify-between mx-auto ">
            {products?.map((itemList, i) => {
              return (
                <div className="z-40" key={itemList?.id}>
                  {
                    cardView === "cardview" ?
                      <ProductCard
                        isLast={i === products.length - 1}
                        nextPage={nextPage}
                        dir={lang.dir}
                        data={itemList}
                        class="z-50 "
                      />
                    :
                      <ProductListCard
                        isLast={i === products.length - 1}
                        nextPage={nextPage}
                        dir={lang.dir}
                        data={itemList}
                        class="z-50 "
                      />
                  }
                  
                </div>
              );
            })}
          </div> */}

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
