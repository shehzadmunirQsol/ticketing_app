import React, { useEffect, useState } from 'react';
import BannerTitle from '~/components/common/banner_title';
import WinnarsBg from '~/public/assets/winner_page.svg';
import CashBg from '~/public/assets/cash_bg.png';
import Cash from '~/public/assets/cash-1.png';
import { useSelector } from 'react-redux';
import { RootState } from '~/store/store';
import WinnarsCard from '~/components/app/winnars/winnars_card';
import Glow from '~/components/common/glow';
import { trpc } from '~/utils/trpc';

export const Winners = () => {
  const { lang } = useSelector((state: RootState) => state.layout);
  const [products, setProducts] = useState<Array<any>>([]);
  const [filters, setFilters] = useState({
    lang_id: lang.lang_id,
    first: 0,
    rows: 9,
    category_id: 2,
  });

  // const {
  //   data: prductsList,
  //   isFetched,
  //   isLoading,
  //   isError,
  // } = trpc.event.getByCategoryId.useQuery(filters, {
  //   refetchOnWindowFocus: false,
  // });

  // const {
  //   data: winnerList,
  //   // isFetched,
  //   // isLoading,
  //   // isError,
  // } = trpc.winner.getWinnersById.useQuery({2}, {
  //   refetchOnWindowFocus: false,
  // });

const customer_id  =2;
  const { data, refetch } = trpc.winner.getWinnersById.useQuery(
    { customer_id },
    {
      refetchOnWindowFocus: false,
      onSuccess: () => {
        console.log({ data });
        // setCarSlider(BannerApiData || []);
      },
      // enabled: user?.id ? true : false,
    },
  );
  console.log(data,"winnerList")



  // useEffect(() => {
  //   if (filters.first > 0 && prductsList?.data?.length) {
  //     setProducts([...products, ...prductsList?.data]);
  //   } else if (prductsList?.data?.length) {
  //     setProducts(prductsList?.data);
  //   }
  // }, [prductsList]);

  // function nextPage() {
  //   console.log('Next page emitted');
  //   if (products.length % filters.rows === 0) {
  //     setFilters({ ...filters, first: 1 + filters.first });
  //   }
  // }

  return (
    <div>
      <div className="relative pt-24"></div>
      <BannerTitle image={WinnarsBg} text={'winners'} />
      <div className="h-full  px-10 py-20">
        <Glow className=" absolute  top-[560px] -right-16  p-2   w-1/5 h-[350px]  " />
        <Glow className=" absolute  bottom-96 -right-16  w-1/5 h-[350px] " />
        {/* <div className=" grid grid-cols-1 md:grid-cols-2     lg:grid-cols-3   justify-between max-w-[1300px] mx-auto ">
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
        </div> */}

        {/* doudt should it load more on action or automatically */}
        {/* {products.length != prductsList?.count ? (
          <div className="w-fit mx-auto">
            <div className="text-center my-4">
              <p className="tracking-tight font-bold">Load More</p>
              <i className="fas fa-arrow-down  text-teal-400 text-5xl my-2  "></i>
            </div>
          </div>
        ) : (
          ''
        )} */}
      </div>
    </div>
  );
};