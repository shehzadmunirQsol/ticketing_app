import Image from 'next/image';
import React, { useState } from 'react';
import { Button } from '~/components/ui/button';
import { Separator } from '~/components/ui/separator';
import Current from '~/public/assets/not-current-entrie.png';
import { trpc } from '~/utils/trpc';

const grid = ['', ''];
// { control: Function }
const AccountView = ({ control }: any) => {
  const { data: customer, isLoading } = trpc.customer.get.useQuery();
  console.log({ customer }, 'customer');

  return (
    <div className="py-4 px-6 text-[#eaeaea]">
      <p className="mb-3">
        Hello{' '}
        <span className="font-bold">{`${customer?.data.first_name} ${customer?.data.last_name}`}</span>
      </p>
      <p>
        From your account dashboard you can view your recent orders, manage your{' '}
        <span
          className="underline cursor-pointer"
          onClick={() => {
            control(1);
          }}
        >
          shipping and billing addresses
        </span>{' '}
        , and{' '}
        <span
          className="underline cursor-pointer"
          onClick={() => {
            control(2);
          }}
        >
          edit your password and account details.
        </span>
      </p>

      <Separator className="my-6" />

      <p className="text-xl font-bold leading-tight  ">
        Your Competition Entries
      </p>
      <p className="text-xs my-2">Showing entries for last 30 days</p>
      <p className="text-base my-2">
        Once you enter a competition your tickets will appear here.{' '}
        <span className="font-bold">Good luck!</span>
      </p>

      <CurrentandPast data={grid} />
    </div>
  );
};

export default AccountView;

function CurrentandPast(data: any) {
  const [select, setSelect] = useState(0);
  return (
    <>
      <div className={`flex  z-10 `}>
        <div className="flex w-fit  ">
          <div
            onClick={() => setSelect(0)}
            className={`p-4 border-[1px] rounded-none text-lg font-black ${
              select == 0
                ? 'border-[#808080]  border-b-transparent text-primary rounded-t-md'
                : 'border-transparent border-b-[#808080] text-[#808080]'
            } `}
          >
            Current
          </div>
          <div
            onClick={() => setSelect(1)}
            className={`p-4 text-center rounded-none border-[1px] text-lg font-black overflow-hidden ${
              select == 1
                ? 'border-[#808080]  border-b-transparent rounded-t-md text-primary'
                : 'border-transparent border-b-[#808080] text-[#808080]'
            } `}
          >
            Part
          </div>
        </div>

        <div className="border-[1px] border-b-[#808080] w-full border-transparent "></div>
      </div>

      <div className="w-full py-4 border-[1px] border-t-0 border-[#808080] h-72 rounded-b-md">
        {data.length != 0 ? (
          <div className="flex flex-col my-auto h-full items-center justify-center">
            <Image src={Current} alt="/" />
            <p className="text-center text-gray-300 text-md my-2 px-6">
              No past competition entries to show. Only entries from the last 30
              days will be shown.
            </p>
            <Button
              variant={'rounded'}
              className="text-center font-black tracking-tighter my-4"
            >
              EXPLORE CURRENT COMPETITIONS
            </Button>
          </div>
        ) : (
          ''
        )}
      </div>
    </>
  );
}
