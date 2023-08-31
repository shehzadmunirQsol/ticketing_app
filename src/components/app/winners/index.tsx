import React, { useEffect, useState } from 'react';
import BannerTitle from '~/components/common/banner_title';
import WinnarsBg from '~/public/assets/winner_page.svg';
import CashBg from '~/public/assets/cash_bg.png';
import Cash from '~/public/assets/cash-1.png';
import { useSelector } from 'react-redux';
import { RootState } from '~/store/store';
import WinnarsCard from '~/components/app/winners/winnars_card';
import Glow from '~/components/common/glow';
import { trpc } from '~/utils/trpc';

export const Winners = () => {
  const { lang } = useSelector((state: RootState) => state.layout);
  const [products, setProducts] = useState<Array<any>>([]);
  const [filters, setFilters] = useState({
    lang_id: lang.lang_id,
    first: 0,
    rows: 9,
  });

  const { data:winnersList, refetch } = trpc.winner.getWinnersById.useQuery(
    filters ,
    {
      refetchOnWindowFocus: false,
      // enabled: user?.id ? true : false,
    },
  );
  console.log(winnersList,"winnerList")



  useEffect(() => {
    if (filters.first > 0 && winnersList?.data?.length) {
      setProducts([...products, ...winnersList?.data]);
    } else if (winnersList?.data?.length) {
      setProducts(winnersList?.data);
    }
  }, [winnersList]);

  function nextPage() {
    console.log('Next page emitted');
    if (products.length % filters.rows === 0) {
      setFilters({ ...filters, first: 1 + filters.first });
    }
  }
console.log(products,"state")
  return (
    <div>
      <div className="relative pt-24 "></div>
      <BannerTitle image={WinnarsBg} text={'winners'} />
      <div className="h-full px-10 py-20">
        <Glow className=" absolute  top-[560px] -left-16  p-2   w-1/5 h-[350px]  " />
        <Glow className=" absolute bottom-[750px]  lg:bottom-[440px] md:bottom-[440px] -right-16  w-1/5 h-[350px] -z-2 " />
        <div className=" grid grid-cols-1 md:grid-cols-2     lg:grid-cols-3   justify-between max-w-[1500px] mx-auto ">
          {products?.map((itemList, i) => {
            return (
              <div className="mx-auto py-2 md:py-0" key={i}>
                <WinnarsCard
                  isLast={i === products.length - 1}
                  nextPage={nextPage}
                  dir={lang.dir}
                  cash={Cash}
                  data={itemList}
                  class="z-50 h-full max-w-sm lg:max-w-2xl md:scale-95  w-full  "
                />
              </div>
            );
          })}
        </div>

        {/* doudt should it load more on action or automatically */}
        {products.length != winnersList?.count ? (
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
    </div>
  );
};
