import React, { useEffect, useState } from 'react';
import { useRouter } from 'next/router';
import { useSelector } from 'react-redux';
import OrdersDataByIdTable from '~/components/common/table/ordersByIdTable';
import Image from 'next/image';
import { Button } from '~/components/ui/button';
import { Separator } from '~/components/ui/separator';
import Current from '~/public/assets/not-current-entrie.png';
import { RootState } from '~/store/store';
import langContent from '~/locales';


const AccountView = ({ control }: any) => {
  const { lang } = useSelector((state: RootState) => state.layout);
  const { user } = useSelector((state: RootState) => state.auth);
  return (
    <div className="py-4 px-6 text-[#eaeaea]">
      <p className="mb-3">
        {langContent[lang.lang].MyAccount.AccountView.HELLO}{}
        <span className="font-bold">
          {user && `${user?.first_name ?? ''} ${user?.last_name ?? ''}`}
        </span>
      </p>
      <p>
      {langContent[lang.lang].MyAccount.AccountView.ACCOUNT}
        <span
          className="underline cursor-pointer"
          onClick={() => {
            control(1);
          }}
        >
          {langContent[lang.lang].MyAccount.AccountView.ADDRESS}
        </span>{' '}
        , {langContent[lang.lang].MyAccount.AccountView.AND}{' '}
        <span
          className="underline cursor-pointer"
          onClick={() => {
            control(2);
          }}
        >
          {langContent[lang.lang].MyAccount.AccountView.EDIT}
        </span>
      </p>

      <Separator className="my-6" />

      <p className="text-xl font-bold leading-tight  ">
      {langContent[lang.lang].MyAccount.AccountView.ENTRIES}
      </p>
      <p className="text-xs my-2">{langContent[lang.lang].MyAccount.AccountView.DAYS}</p>
      <p className="text-base my-2">
      {langContent[lang.lang].MyAccount.AccountView.COMPETITION}{' '}
        <span className="font-bold">{langContent[lang.lang].MyAccount.AccountView.LUCK}</span>
      </p>
      <CurrentandPast customer_id={user?.id} />
    </div>
  );
};

export default AccountView;

interface currentandpastprops {
  customer_id?: number;
}
function CurrentandPast({ customer_id }: currentandpastprops) {
  const { lang } = useSelector((state: RootState) => state.layout);

  const [select, setSelect] = useState(0);
  const [filters, setFilters] = useState({
    customer_id: customer_id,
    status: 'current',
    first: 0,
    rows: 5,
    lang_id: 1,
  });


  const router = useRouter();
  useEffect(() => {
    if (select === 0) {
      setFilters({
        customer_id: customer_id,
        status: 'current',
        first: 0,
        rows: 5,
        lang_id: 1,
      });
    } else {
      setFilters({
        customer_id: customer_id,
        status: 'past',
        first: 0,
        rows: 5,
        lang_id: 1,
      });
    }
  }, [select]);

  return (
    <>
      <div className={`flex  z-10 `}>
        <div className="flex w-fit  cursor-pointer">
          <div
            onClick={() => setSelect(0)}
            className={`p-4 border-[1px] rounded-none text-lg font-black ${
              select == 0
                ? 'border-[#808080]  border-b-transparent text-primary rounded-t-md'
                : 'border-transparent border-b-[#808080] text-[#808080]'
            } `}
          >
            {langContent[lang.lang].MyAccount.AccountView.CURRENT}
          </div>
          <div
            onClick={() => setSelect(1)}
            className={`p-4 text-center rounded-none border-[1px] text-lg font-black overflow-hidden ${
              select == 1
                ? 'border-[#808080]  border-b-transparent rounded-t-md text-primary'
                : 'border-transparent border-b-[#808080] text-[#808080]'
            } `}
          >
                        {langContent[lang.lang].MyAccount.AccountView.PAST}
          </div>
        </div>

        <div className="border-[1px] border-b-[#808080] w-full border-transparent "></div>
      </div>

      <div className="w-full p-4 border-[1px] border-t-0 border-[#808080] h-fit rounded-b-md">
        {customer_id != undefined ? (
          <>
            {select === 0 ? (
              <OrdersDataByIdTable filters={filters} setFilters={setFilters} />
            ) : (
              <OrdersDataByIdTable filters={filters} setFilters={setFilters} />
            )}
          </>
        ) : (
          <>
            <div className="flex flex-col my-auto h-full items-center justify-center">
              <Image src={Current} alt="/" />
              <p className="text-center text-gray-300 text-md my-2 px-6">
              {langContent[lang.lang].MyAccount.AccountView.INFO}

              </p>
              <Button
                variant={'rounded'}
                className="text-center font-black tracking-tighter my-4 w-full h-fit text-xs sm:w-fit md:text-md "
                onClick={() => router.push('/cars')}
              >
                              {langContent[lang.lang].MyAccount.AccountView.INFO_HEADING}

              </Button>
            </div>
          </>
        )}
      </div>
    </>
  );
}
