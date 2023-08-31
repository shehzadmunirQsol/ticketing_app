import React, { useState } from 'react';
import BannerTitle from '~/components/common/banner_title';
import BackgroundImage from '~/public/assets/account.png';
import AccountView from '~/components/app/account/account-view';
import AddressesView from './address-view';
import AccountDetails from './account-details';
import { trpc } from '~/utils/trpc';

const Account = () => {
  const [counter, setCounter] = useState(0);
  
  const navigation = [
    {
      tab: 'Dashboard',
      title: 'MY ACCOUNT',
      view: <AccountView control={setCounter} />,
    },
    {
      tab: 'Addresses',
      title: 'ADDRESSES',
      view: <AddressesView />,
    },
    {
      tab: 'Account Details',
      title: 'ACOUNT DETAILS',
      view: <AccountDetails />,
    },
    // phase 2
    // {
    //   tab:"Safe Playing",
    //   title:"SAFE PLAYING",
    // },
    {
      tab: 'Logout',
      fn: '',
    },
  ];

  return (
    <>
      <div className="relative pt-24"></div>
      <BannerTitle image={BackgroundImage} text={navigation[counter]?.title} />
      <div className=" py-10 max-w-[1600px] md:px-16 px-4 mx-2 sm:mx-auto  flex  flex-col sm:flex-row justify-start sm:justify-between gap-8 items-start">
        <ul className="bg-[#101417]   w-full sm:w-96   rounded-lg overflow-hidden ">
          {navigation.map((item, i) => (
            <li
              key={i}
              className={`border-b-[0.5px] p-4  border-b-[#1B1D1F] last:border-b-none cursor-pointer border-l-4 ${
                counter === i
                  ? 'bg-[#1B1D1F]  border-l-primary text-primary '
                  : 'border-l-transparent'
              } `}
              onClick={() => setCounter(i)}
            >
              <div
                className={`${
                  counter === i ? 'text-primary' : 'text-[#808080]'
                } font-[800] tracking-tight`}
              >
                {item.tab}
              </div>
            </li>
          ))}
        </ul>

        <div className="w-full  bg-[#101417] mx-auto p-4 rounded-sm ">
          {navigation[counter]?.view}
        </div>
      </div>
    </>
  );
};

export default Account;
