'use client';
import React, { useEffect, useInsertionEffect, useRef, useState } from 'react';
import BannerTitle from '~/components/common/banner_title';
import CashBg from '~/public/assets/cash_bg.png';
import Cash from '~/public/assets/cash-1.png';
import { useSelector } from 'react-redux';
import { RootState } from '~/store/store';
import ProductCard from '~/components/common/card';
import Glow from '~/components/common/glow';
// import {}
import { useInfiniteQuery } from '@tanstack/react-query';
import { useIntersection } from '@mantine/hooks';
import { trpc } from '~/utils/trpc';

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

const fetchPost = async (page: number, data: Array<any>) => {
  await new Promise((resolve) => setTimeout(resolve, 1000));
  return data.slice((page - 1) * 9, page * 9);
};

const CashPage = () => {
  const { lang } = useSelector((state: RootState) => state.layout);

  const [initialData, setData] = useState({
    first: 0,
    rows: 9,
  });

  function nextPage() {
    console.log('Next page emitted');
    setData({ ...initialData, first: initialData.first + 1 });
  }
  // const {data:video}=trpc.

  const designList = ['', '', '', '', ''];
  const { data, fetchNextPage, isFetchingNextPage } = useInfiniteQuery(
    ['cashProductQuery'],
    async ({ pageParam = 1 }) => {
      const response = await fetchPost(pageParam, cashList);
      return response;
    },
    {
      getNextPageParam: (_, pages) => {
        //get the next cursor for infinite query
        return pages.length + 1;
      },
      initialData: {
        pages: [cashList.slice(0, 9)],
        pageParams: [1],
      },
    },
  );

  console.log({ data }, 'data');
  const _product = data?.pages?.flatMap((product) => product);

  const lastProductRef = useRef<HTMLElement>(null);
  const { ref, entry } = useIntersection({
    root: lastProductRef.current,
    threshold: 1,
  });

  useEffect(() => {
    if (entry?.isIntersecting) fetchNextPage();
  }, [entry]);


  return (
    <>
      <BannerTitle image={CashBg} text={'Cash'} />
      <div className="h-full  px-10 py-20">
        <Glow className=" absolute  top-[560px] -right-16  p-2   w-1/5 h-[350px]  " />
        <Glow className=" absolute  bottom-96 -right-16  w-1/5 h-[350px] " />
        <div className=" grid grid-cols-1 md:grid-cols-2     lg:grid-cols-3   justify-between max-w-[1300px] mx-auto ">
          {_product?.map((itemList, i) => {
            console.log({ itemList }, 'item');
            if (i === _product.length - 1) {
              return (
                <div className="mx-auto py-2 md:py-0" key={i} ref={ref}>
                  <ProductCard
                    isLast={i === _product.length - 1}
                    nextPage={nextPage}
                    dir={lang.dir}
                    cash={itemList?.src}
                    class="z-50 h-full max-w-sm lg:max-w-2xl md:scale-95  w-full  "
                  />
                </div>
              );
            }
            return (
              <div className="mx-auto py-2 md:py-0" key={i}>
                <ProductCard
                  isLast={i === _product.length - 1}
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

export default CashPage;
