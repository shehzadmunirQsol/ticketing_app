import React, { useState } from 'react';
import BannerTitle from '~/components/common/banner_title';
import BackgroundImage from '~/public/assets/account.png';
import AccountView from '~/components/app/account/account-view';
import AddressesView from './address-view';
import AccountDetails from './account-details';
import { trpc } from '~/utils/trpc';
import { useToast } from '~/components/ui/use-toast';
import { useRouter } from 'next/router';
import { useDispatch, useSelector } from 'react-redux';
import { addCart } from '~/store/reducers/cart';
import { userLogout } from '~/store/reducers/auth';
import langContent from '~/locales';
import { RootState } from '~/store/store';

const Account = () => {
  const { toast } = useToast();
  const router = useRouter();
  const dispatch = useDispatch();

  const { lang } = useSelector((state: RootState) => state.layout);

  const [counter, setCounter] = useState(0);

  const logout = trpc.customer.logout.useMutation({
    onSuccess: (res: any) => {
      console.log('return data', res);
    },
    onError(error) {
      console.log(error.message, 'ERROR');
    },
  });

  async function handleLogout() {
    try {
      const response = await logout.mutateAsync({});
      console.log('Response : ', response);
      toast({
        variant: 'success',
        title: 'Logout successfully! ',
      });
      localStorage.removeItem('winnar-token');
      localStorage.removeItem('customer');
      dispatch(userLogout());
      dispatch(
        addCart({
          id: null,
          customer_id: null,
          isDiscount: false,
          discount: 0,
          isPercentage: false,
          cartItems: [],
        }),
      );
      router.push('/login');
    } catch (error: any) {
      console.log('Error ', error);
      toast({
        variant: 'destructive',
        title: error.message,
      });
    }
  }

  const renderComponents: any = {
    0: <AccountView control={setCounter} />,
    1: <AddressesView />,
    2: <AccountDetails />,
  };

  return (
    <>
      <div className="relative pt-24"></div>
      <BannerTitle
        image={BackgroundImage}
        text={langContent[lang.lang].MyAccount.array[counter]?.title}
      />
      <div className="relative py-10 max-w-[1600px] md:px-16 px-4 mx-2 sm:mx-auto  flex  flex-col mdx:flex-row justify-start sm:justify-between gap-8 items-start">
        <ul className="sticky top-36  bg-[#101417]   w-full mdx:w-96   rounded-lg ">
          {langContent[lang.lang].MyAccount.array.map((item, i) => {
            return (
              <li
                key={i}
                className={`border-b-[0.5px] p-4  border-b-[#1B1D1F] last:border-b-none cursor-pointer border-l-4 ${
                  counter === i
                    ? 'bg-[#1B1D1F]  border-l-primary text-primary '
                    : 'border-l-transparent'
                } `}
                onClick={() =>
                  item.tab === 'Logout' || item.tab === 'تسجيل خروج'
                    ? handleLogout()
                    : setCounter(i)
                }
              >
                <div
                  className={`${
                    counter === i ? 'text-primary' : 'text-[#808080]'
                  } font-[800] tracking-tight`}
                >
                  {item.tab}
                </div>
              </li>
            );
          })}
        </ul>

        <div className="w-full  bg-[#101417] mx-auto p-4 rounded-sm ">
          {renderComponents[counter]}
        </div>
      </div>
    </>
  );
};

export default Account;
