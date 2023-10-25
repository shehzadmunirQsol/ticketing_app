import React, { useState } from 'react';
import { Button } from '@/ui/button';
import Image from 'next/image';
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

interface LinkItemProps {
  name: string;
  link: string;
  icon: string;
  disable?: boolean;
}
function Footer() {
  const { lang } = useSelector((state: RootState) => state.layout);
  const router = useRouter();
  const [isModal, setIsModal] = useState(false);

  return (
    <footer className="h-full  bg-background-footer !z-50">
      <div className="relative w-full max-w-screen max-w-[1600px] mx-auto h-full ">
        <div className=" absolute  p-1 w-1/2 z-20  h-full  ">
          <Glow className="absolute bottom-0 -left-16 z-20  p-2   w-1/2 h-1/3  " />
          <div className="absolute top-0   p-2  w-full h-full z-4   ">
            <Image
              className="w-full h-full object-contain "
              src={Lines}
              quality={100}
              alt="Sunset in the mountains"
            />
          </div>
        </div>
        <div className="relative z-30 ">
          <div className="relative p-4 py-6 md:px-14 md:py-14">
            <div className="sm:flex sm:justify-between ">
              <div className=" mb-6 md:mb-0 flex flex-col gap-8 w-100 sm:w-[300px] lg:w-[360px]">
                <div>
                  {/* <Button
                    variant="rounded-outline"
                    className="z-30 font-bold bg-transparent min-w-max "
                    onClick={() => { router.push("https://bdc4c4ca.sibforms.com/serve/MUIFAM1e-fZfnXl_ZoAGyLbE2vO-dB_qtovXSlb52nP7BXPR5bjPlLtbMAYS3gwKqMo1BAbEAHnopSyyntSfymgYib7FkxJ85zHDX6h_8TKpFb1UjrMFK_Hjm2HFqozFIIf-m_2RfEEKiurCXjBtZkpbK6wL-n48VpeJBjDwQggOQ-UYU7GHx0Nzzu1BAmj0Rg3cswAawX5IfFDn") }}
                  >
                    <div>
                      {langContent[lang.lang].Footer.SUBSCRIBE_BTN}
                      &nbsp;
                      <i className="fa-solid fa-arrow-right -rotate-45 "></i>
                    </div>
                  </Button> */}
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
                <div className="w-full opacity-75 text-sm text-colorGray  ">
                  <p>{langContent[lang.lang].Footer.ADDRESS_TITLE}</p>
                  <p>{langContent[lang.lang].Footer.ADDRESS_SUB_TITLE}</p>
                  <p>{langContent[lang.lang].Footer.ADDRESS_SUB_TITLE_ONE}</p>
                  <p>{langContent[lang.lang].Footer.ADDRESS_SUB_TITLE_TWO}</p>
                  <p>{langContent[lang.lang].Footer.ADDRESS_SUB_TITLE_THREE}</p>
                  <p className="text-xs my-2 hidden sm:block">
                    {langContent[lang.lang].Footer.ADDRESS_SUB_TITLE_FOUR}
                  </p>
                </div>
                <div className=" flex items-center justify-start md:justify-start  text-sm text-white gap-3 sm:gap-4">
                  <Image
                    className="h-full object-contain "
                    src={visa}
                    quality={100}
                    alt="Sunset in the mountains"
                  />
                  <Image
                    className="h-full object-contain "
                    src={master}
                    quality={100}
                    alt="Sunset in the mountains"
                  />
                  <Image
                    className="h-full object-contain "
                    src={Paypal}
                    quality={100}
                    alt="Sunset in the mountains"
                  />
                  <Image
                    className="h-full object-contain "
                    src={applePay}
                    quality={100}
                    alt="Sunset in the mountains"
                  />
                </div>
              </div>
              <div className=" grid grid-cols-1 gap-8 md:gap-6 md:grid-cols-2 w-full sm:w-[300px] lg:w-[360px]">
                <div className="col-span-2 flex items-center justify-between">
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

          <div className="flex flex-col px-4 sm:mt-8 md:px-8 w-full relative justify-end items-end gap-3">
            <Image
              className="w-full h-full object-contain  z-40 "
              src={Group16}
              quality={100}
              alt="Sunset in the mountains"
            />
            <p className="text-xs my-2 block sm:hidden">
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
      name: 'Winners',
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
