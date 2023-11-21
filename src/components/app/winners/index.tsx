import React, { useEffect, useRef, useState } from 'react';
import BannerTitle from '~/components/common/banner_title';
import WinnarsBg from '~/public/assets/winner_page.svg';
import Cash from '~/public/assets/cash-1.png';
import { useSelector } from 'react-redux';
import { RootState } from '~/store/store';
import WinnarsCard from '~/components/app/winners/winnars_card';
import Glow from '~/components/common/glow';
import { trpc } from '~/utils/trpc';
import langContent from '~/locales';
import { useDebounce } from '~/hooks/useDebounce';

export const Winners = () => {
  const { lang } = useSelector((state: RootState) => state.layout);
  const [products, setProducts] = useState<Array<any>>([]);
  const [search, setSearch] = useState<string>('');

  const [filters, setFilters] = useState({
    lang_id: lang.lang_id,
    first: 0,
    rows: 9,
    filters: {
      searchQuery: '',
    },
  });

  const inputRef = useRef<HTMLInputElement>(null);
  const searchQuery = useDebounce<string>(search, 300);

  const { data: winnersList, isFetching } = trpc.winner.get.useQuery(filters, {
    refetchOnWindowFocus: false,
    onSuccess(data) {
      if (filters.first > 0) {
        setProducts([...products, ...data.data]);
      } else {
        setProducts(data.data);
      }
    },
  });

  useEffect(() => {
    setFilters({
      lang_id: lang.lang_id,
      first: 0,
      rows: 9,
      filters: { searchQuery: searchQuery?.trim() },
    });
  }, [searchQuery]);

  function nextPage() {
    if (
      winnersList?.data?.length === filters.rows &&
      winnersList?.count > products.length
    ) {
      setFilters({ ...filters, first: ++filters.first });
    }
  }

  function searchHandler(e: React.ChangeEvent<HTMLInputElement>) {
    const value = e.target.value?.trimStart();
    if (value?.length <= 72) {
      setSearch(value);
    }
  }

  return (
    <div>
      <div className="relative pt-24 "></div>
      <BannerTitle
        image={WinnarsBg}
        text={langContent[lang.lang].Winners.HEADING}
      />
      <div className="relative h-full px-4 py-8 md:px-14 md:py-20">
        <Glow className=" absolute  -top-10 -left-16  p-2   w-1/6 h-[150px]" />
        <Glow className=" absolute -bottom-10 -right-16  w-1/6 h-[150px] -z-10 " />
        <div className="mb-6 grid gap-6 grid-cols-1 md:grid-cols-2 lg:grid-cols-3 ">
          <div className="hidden lg:block" />
          <div className="hidden md:block" />
          <div
            onClick={() => inputRef?.current && inputRef?.current?.focus()}
            className="px-4 py-2 gap-4 rounded-sm bg-background-footer border-border flex items-center cursor-pointer"
          >
            <i className="fa fa-search text-xl" />
            <input
              ref={inputRef}
              onChange={searchHandler}
              value={search}
              className="flex-1 bg-transparent border-none outline-none"
              placeholder="Search your friends and other lucky people... "
            />
            {search ? (
              <i
                onClick={() => setSearch('')}
                className="fa fa-xmark text-xl"
              />
            ) : null}
          </div>
        </div>
        {isFetching && filters.first === 0 ? (
          <div className="my-20 mx-auto loader text-center text-2xl md:text-4xl lg:text-5xl ease-linear rounded-full border-4 border-t-4 border-gray-200 h-12 w-12" />
        ) : products.length == 0 ? (
          <h2 className="py-20 text-center text-2xl md:text-4xl lg:text-5xl font-black">
            {filters.filters.searchQuery ? (
              <>
                <span>No results for: </span>
                <span className="text-primary">
                  {filters.filters.searchQuery}
                </span>
              </>
            ) : (
              `No winners selected yet`
            )}
          </h2>
        ) : (
          <div className="grid gap-6 grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 ">
            {products?.map((itemList: any, i: any) => {
              return (
                <div key={i}>
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
