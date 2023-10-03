import React, { useState } from 'react';
import BannerTitle from '~/components/common/banner_title';
import WinnarsBg from '~/public/assets/winner_page.svg';
import Cash from '~/public/assets/cash-1.png';
import { useSelector } from 'react-redux';
import { RootState } from '~/store/store';
import WinnarsCard from '~/components/app/winners/winnars_card';
import Glow from '~/components/common/glow';
import { trpc } from '~/utils/trpc';
import langContent from '~/locales';

export const Winners = () => {
  const { lang } = useSelector((state: RootState) => state.layout);
  const [products, setProducts] = useState<Array<any>>([]);
  const [filters, setFilters] = useState({
    lang_id: lang.lang_id,
    first: 0,
    rows: 9,
  });

  const { data: winnersList } = trpc.winner.get.useQuery(filters, {
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
      winnersList?.data?.length === filters.rows &&
      winnersList?.count > products.length
    ) {
      setFilters({ ...filters, first: ++filters.first });
    }
  }

  return (
    <div>
      <div className="relative pt-24 "></div>
      <BannerTitle image={WinnarsBg} text={langContent[lang.lang].Winners.HEADING}/>
      <div className="relative h-full px-4 md:px-14  py-20">
        <Glow className=" absolute  -top-10 -left-16  p-2   w-1/6 h-[150px]" />
        <Glow className=" absolute -bottom-10 -right-16  w-1/6 h-[150px] -z-10 " />
        {products.length == 0 ? (
          <h2 className="py-20 md:py-40 lg:py-48 text-center text-2xl md:text-4xl lg:text-5xl font-black uppercase">
            No Winners Selected yet
          </h2>
        ) : (
          <div className="grid gap-6 grid-cols-1 md:grid-cols-2 lg:grid-cols-3 ">
            {products?.map((itemList: any, i: any) => {
              return (
                <div className="m-auto" key={i}>
                  <WinnarsCard
                    isLast={i === products.length - 1}
                    nextPage={nextPage}
                    dir={lang.dir}
                    cash={Cash}
                    data={itemList}
                    class=""
                  />
                </div>
              );
            })}
          </div>
        )}
      </div>
    </div>
  );
};
