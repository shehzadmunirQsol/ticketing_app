import React, { useState } from 'react';
import { Button } from '@/ui/button';
import Lines from '~/public/assets/icons/Lines_Big.png';
import Group16 from '~/public/assets/icons/Group16.svg';
import visa from '~/public/assets/icons/visa.svg';
import master from '~/public/assets/icons/Master.svg';
import Paypal from '~/public/assets/icons/Paypal.svg';
import applePay from '~/public/assets/icons/applePay.svg';
import Glow from '~/components/common/glow';
import Link from 'next/link';
import langContent from '~/locales';
import { useSelector } from 'react-redux';
import { RootState } from '~/store/store';
import { useRouter } from 'next/router';
import { NewsLetterDialog } from '~/components/common/modal/newsLetterModal';
import NextImage from '~/components/ui/img';
import { trpc } from '~/utils/trpc';

interface LinkItemProps {
  name: string;
  link: string;
  icon: string;
  disable?: boolean;
}





function Footer() {
  const { lang } = useSelector((state: RootState) => state.layout);
  const { isLogin, user } = useSelector((state: RootState) => state.auth);
  const router = useRouter();
  const [isModal, setIsModal] = useState(false);


  // Brevo Track
  const trackBrevo = (value: string) => {
    if(user?.email){
      if ('sendinblue' in window && window?.sendinblue) {
        const sendinblue: any = window.sendinblue;
        sendinblue?.track(
          'socialmediachannel_click',
          {
            "email": user?.email,
          },
          {
            "data": {
              "channel": value
            }
          },
        ) as any;
      }
    }
  };
  // Brevo Track




  //-------TOTAL TICKET COUNT
  const { data: prductsList } = trpc.eventTicket.getTotalTicketSold.useQuery(undefined, {
    refetchOnWindowFocus: false,
    onSuccess(data: any) {
    },
  });

  var totalnum = 0;
  if (prductsList && prductsList.data) {
    prductsList.data.forEach((item: any) => {
      totalnum += item.quantity;
    });
  }

  const bottlePrice = 0.25;
  const totalSoldTickets = totalnum;
  const charityAmount = (totalSoldTickets * bottlePrice).toLocaleString();
  //-------TOTAL TICKET COUNT



  return (
    <footer className="h-full  bg-background-footer !z-50">
      <div className="relative w-full max-w-screen max-w-[1600px] mx-auto h-full ">
        <div className=" absolute z-0 p-1 w-1/2 h-full  ">
          <Glow className="absolute z-0 bottom-0 -left-16 p-2 w-1/2 h-1/3" />
          <div className="absolute z-0 top-0 p-2 w-full h-full">
            <NextImage
              className="w-full h-full object-contain "
              src={Lines}
              quality={100}
              alt="Sunset in the mountains"
            />
          </div>
        </div>
        <div className="relative">
          <div className="relative p-4 py-6 md:px-14 md:py-14">
            <div className="sm:flex sm:flex-row-reverse sm:justify-between sm:items-center mb-4 md:mb-8">
              <div className="w-100 sm:w-[300px] lg:w-[360px]">
                <p className="text-gray-200 text-base md:text-lg font-semibold leading-[18px] mb-4 md:mb-0">{langContent[lang.lang].Footer.CHARIITY} <span className="text-primary">AED {charityAmount}</span></p>
              </div>
              <div className="flex flex-col gap-6 md:gap-8 w-100 sm:w-[300px] lg:w-[360px]">
                <div>
                  <Button
                    variant="rounded-outline"
                    className="z-30 font-bold bg-transparent min-w-max "
                    onClick={() => setIsModal(true)}
                  >
                    <div>
                      {langContent[lang.lang].Footer.SUBSCRIBE_BTN}
                      &nbsp;
                      <i className="fa-solid fa-arrow-right -rotate-45 "></i>
                    </div>
                  </Button>
                </div>
              </div>
            </div>
            <div className="sm:flex sm:justify-between ">
              <div className="mb-6 md:mb-0 flex flex-col gap-6 md:gap-8 w-100 sm:w-[300px] lg:w-[360px]">
                <div className="w-full opacity-75 text-sm text-colorGray  ">
                  <p>{langContent[lang.lang].Footer.ADDRESS_TITLE}</p>
                  <p>{langContent[lang.lang].Footer.ADDRESS_SUB_TITLE}</p>
                  <p>{langContent[lang.lang].Footer.ADDRESS_SUB_TITLE_ONE}</p>
                  <p>{langContent[lang.lang].Footer.ADDRESS_SUB_TITLE_TWO}</p>
                  <p>{langContent[lang.lang].Footer.ADDRESS_SUB_TITLE_THREE}</p>
                </div>
                <div className="flex items-center justify-start md:justify-start text-sm text-white gap-3 sm:gap-4">
                  <NextImage
                    className="h-full object-contain "
                    src={visa}
                    quality={100}
                    alt="Sunset in the mountains"
                  />
                  <NextImage
                    className="h-full object-contain "
                    src={master}
                    quality={100}
                    alt="Sunset in the mountains"
                  />
                  <NextImage
                    className="h-full object-contain "
                    src={applePay}
                    quality={100}
                    alt="Sunset in the mountains"
                  />
                  {/* <NextImage
                    className="h-full object-contain "
                    src={Paypal}
                    quality={100}
                    alt="Sunset in the mountains"
                  /> */}
                </div>
              </div>
              <div className=" grid grid-cols-1 gap-4 md:gap-6 md:grid-cols-2 w-full sm:w-[300px] lg:w-[360px]">
                <div className="col-span-2 flex items-start justify-between">
                  <ul className="flex-1 text-grayColor opacity-75  space-y-2 sm:space-y-2">
                    {langContent[lang.lang].Footer.array.map((item, index) => {
                      return (
                        <li key={index} className="text-sm">
                          <Link href={item.link} className="hover:underline">
                            {item.name}
                          </Link>
                        </li>
                      );
                    })}
                  </ul>
                  <ul className="flex-1 text-grayColor opacity-75 space-y-2 sm:space-y-2">
                    {langContent[lang.lang].Footer.arrayTwo.map(
                      (item, index) => {
                        return (
                          <li key={index} className="text-sm">
                            <Link href={item.link} className="hover:underline">
                              {item.name}
                            </Link>
                          </li>
                        );
                      },
                    )}
                  </ul>
                </div>

                <div className=" col-span-2 w-full ">
                  <div className="flex-1 flex items-center gap-2 mdx:justify-between">
                    <div className=" text-sm font-bold w-full min-w-max">
                      {langContent[lang.lang].Footer.CONNECT_TITLE}
                    </div>
                    <div className="w-full flex gap-2 items-center">
                      {' '}
                      <Link
                        target="_blank"
                        href="https://www.facebook.com/WinnarUAE"
                      >
                        <Button
                          variant="outline"
                          size="icon_square"
                          className="p-1 rounded-md"
                          onClick={() => trackBrevo('Facebook')}
                        >
                          <i className="fa-brands fa-facebook-f text-xl"></i>
                        </Button>
                      </Link>
                      <Link
                        target="_blank"
                        href="https://www.instagram.com/winnar"
                      >
                        <Button
                          variant="outline"
                          size="icon_square"
                          className=" rounded-md"
                          onClick={() => trackBrevo('Instagram')}
                        >
                          <i className="fa-brands fa-instagram text-xl"></i>
                        </Button>
                      </Link>
                      <Link
                        target="_blank"
                        href="https://www.linkedin.com/company/winnar/"
                      >
                        <Button
                          variant="outline"
                          size="icon_square"
                          className=" rounded-md"
                          onClick={() => trackBrevo('LinkedIn')}
                        >
                          <i className="fa-brands fa-linkedin text-xl"></i>
                        </Button>
                      </Link>
                      <Link
                        target="_blank"
                        href="https://www.youtube.com/@Winnarofficial"
                      >
                        <Button
                          variant="outline"
                          size="icon_square"
                          className="p-1 rounded-md"
                          onClick={() => trackBrevo('YouTube')}
                        >
                          <i className="fa-brands fa-youtube text-xl"></i>
                        </Button>
                      </Link>
                    </div>
                  </div>
                </div>
              </div>
            </div>
          </div>

          <div className="flex flex-col px-4 sm:mt-1 md:px-8 w-full relative gap-3">
            <NextImage
              className="w-full h-full object-contain"
              src={Group16}
              quality={100}
              alt="Sunset in the mountains"
            />
            <p className="w-full text-xs smtext mt-2 mb-3 text-center">
              {langContent[lang.lang].Footer.ADDRESS_SUB_TITLE_FOUR}
            </p>
          </div>
        </div>
      </div>
      <NewsLetterDialog
        isModal={isModal}
        setIsModal={setIsModal}
        title={'Newsletter'}
      />
    </footer>
  );
}

export default Footer;

export function ItemMenuDemo() {
  const linkItems: Array<LinkItemProps> = [
    { name: 'Cars', link: '/', icon: 'fas fa-house' },
    {
      name: 'Cash',
      link: `/#`,
      icon: 'fa-solid fa-globe',
    },
    {
      name: 'Winnars',
      link: `/#`,
      icon: 'fa-sharp fa-regular fa-images',
    },
    {
      name: 'About Us',
      link: `/#`,
      icon: 'fa-solid fa-image',
    },
    {
      name: 'FAQ',
      link: `/faq`,
      icon: 'fa-solid fa-users',
    },
  ];
  return (
    <div
      className="items-center justify-between hidden w-full md:flex md:w-auto "
      id="navbar-sticky"
    >
      <ul className="flex flex-col p-4 md:p-0  text-small font-normal  border  rounded-lg bg-transparent md:flex-row md:space-x-4 md:mt-0 md:border-0 md:bg-white dark:bg-transparent ">
        {linkItems &&
          linkItems?.map((item, index) => {
            return (
              <li key={index} className="group border-b-2 border-transparent  ">
                <a
                  href="#"
                  className="flex flex-col py-2 pl-3 pr-4 text-gray-200   hover:bg-gray-100  md:hover:bg-transparent  dark:text-white dark:hover:bg-gray-700 dark:hover:text-white md:dark:hover:bg-transparent dark:border-gray-700"
                >
                  {item?.name}
                  <span className="w-full h-1 bg-transparent group-hover:bg-teal mt-1 rounded-sm  "></span>
                </a>
              </li>
            );
          })}
      </ul>
    </div>
  );
}
