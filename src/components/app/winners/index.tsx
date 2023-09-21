import React, { useEffect, useState } from 'react';
import BannerTitle from '~/components/common/banner_title';
import WinnarsBg from '~/public/assets/winner_page.svg';
import Cash from '~/public/assets/cash-1.png';
import { useSelector } from 'react-redux';
import { RootState } from '~/store/store';
import WinnarsCard from '~/components/app/winners/winnars_card';
import Glow from '~/components/common/glow';
import { trpc } from '~/utils/trpc';
import { LoadingDialog } from '~/components/common/modal/loadingModal';

export const Winners = () => {
  const { lang } = useSelector((state: RootState) => state.layout);
  const [products, setProducts] = useState<Array<any>>([]);
  const [filters, setFilters] = useState({
    lang_id: lang.lang_id,
    first: 0,
    rows: 9,
  });

  const { data: winnersList, isLoading } = trpc.winner.get.useQuery(filters, {
    refetchOnWindowFocus: false,
  });

  useEffect(() => {
    if (filters.first > 0 && winnersList?.data?.length) {
      if (winnersList?.data?.length) {
        setProducts([...products, ...winnersList?.data]);
      }
    } else if (winnersList?.data?.length) {
      setProducts(winnersList?.data);
    }
  }, [winnersList]);

  function nextPage() {
    if (products.length % filters.rows === 0) {
      setFilters({ ...filters, first: 1 + filters.first });
    }
  }

  return (
    <div>
      <div className="relative pt-24 "></div>
      <BannerTitle image={WinnarsBg} text={'winners'} />
      <div className="relative h-full px-10 py-20">
        <Glow className=" absolute  -top-10 -left-16  p-2   w-1/6 h-[150px]" />
        <Glow className=" absolute -bottom-10 -right-16  w-1/6 h-[150px] -z-10 " />
        {products.length == 0 ? (
          <h2 className="py-20 md:py-40 lg:py-48 text-center text-2xl md:text-4xl lg:text-5xl font-black uppercase">
            No Winners Selected yet
          </h2>
        ) : (
          <div className="grid gap-2 grid-cols-1 md:grid-cols-2 lg:grid-cols-3 max-w-[1500px] m-auto">
            {products?.map((itemList: any, i: any) => {
              return (
                <div className="m-auto" key={i}>
                  <WinnarsCard
                    isLast={i === products.length - 1}
                    nextPage={nextPage}
                    dir={lang.dir}
                    cash={Cash}
                    data={itemList}
                    class="h-full max-w-sm lg:max-w-2xl md:scale-95 w-full"
                  />
                </div>
              );
            })}
          </div>
        )}
      </div>
      <LoadingDialog open={isLoading} text={'Loading...'} />
    </div>
  );
};
