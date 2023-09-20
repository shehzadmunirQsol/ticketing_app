import React from 'react';
import { Button } from '@/ui/button';
import Image from 'next/image';
import Lines from '~/public/assets/icons/Lines_Big.png';
import Group16 from '~/public/assets/icons/Group16.svg';
import Group17 from '~/public/assets/icons/Group17.png';
import Glow from '~/components/common/glow';
import Link from 'next/link';

interface LinkItemProps {
  name: string;
  link: string;
  icon: string;
  disable?: boolean;
}
function Footer() {
  return (
    <footer className="h-full  bg-background-footer !z-50">
      <div className="relative w-full max-w-screen max-w-[1600px] mx-auto h-full ">
        <div className=" absolute  p-1 w-1/2 z-20  h-full  ">
          <Glow className="absolute bottom-0 -left-16 z-20  p-2   w-1/2 h-1/3  " />
          {/* <div className="absolute bottom-0 -left-16 z-2  p-2  w-1/2 h-1/3  bg-teal-400 bg-opacity-30 rounded-full blur-3xl"></div> */}
          {/* <div className="absolute bottom-0 -left-16 z-4  p-2  w-1/3 h-1/3  bg-teal-400 bg-opacity-10 rounded-full blur-3xl"></div> */}
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
            <div className="md:flex md:justify-between ">
              <div className="mb-6 md:mb-0 flex flex-col gap-8 lg:w-2/3">
                <div>
                  <Button
                    variant="rounded-outline"
                    className="z-30 font-bold bg-transparent"
                  >
                    <div>
                      SUBSCRIBE TO NEWSLETTER &nbsp;
                      <i className="fa-solid fa-arrow-right -rotate-45 "></i>
                    </div>
                  </Button>
                </div>
                <div className="w-full opacity-75 text-sm text-colorGray">
                  <p>© Copyright WINNAR 2023</p>
                  <p>Registered Company </p>
                  <p>Number: 11320154, UAE</p>
                </div>
                <div className="m-auto flex items-center justify-center w-full sm:w-1/2 md:w-1/3 text-sm text-white">
                  <Image
                    className="w-full h-full object-contain text-center"
                    src={Group17}
                    quality={100}
                    alt="Sunset in the mountains"
                  />
                </div>
              </div>
              <div className="grid grid-cols-1 gap-8 md:gap-6 md:grid-cols-2 w-full md:w-1/3">
                <ul className="text-grayColor opacity-75 space-y-3">
                  {[
                    { page: 'Home', link: '/' },
                    { page: 'Cars', link: '/cars' },
                    { page: 'Cash', link: '/cash' },
                    { page: 'Winners', link: '/winners' },
                    { page: 'FAQs', link: '/faq' },
                  ].map((item, index) => {
                    return (
                      <li key={index} className="text-sm">
                        <a href={item.link} className="hover:underline">
                          {item.page}
                        </a>
                      </li>
                    );
                  })}
                </ul>
                <ul className="text-grayColor opacity-75 space-y-3">
                  {[
                    { page: 'About Us', link: '/about-us' },
                    { page: 'Contact Us', link: '/contact-us' },
                    { page: 'Terms & Conditions', link: '/terms-condition' },
                    { page: 'Our Team', link: '/about-us' },
                    {
                      page: 'Cookie & Privacy Policy',
                      link: '/privacy-policy',
                    },
                  ].map((item, index) => {
                    return (
                      <li key={index} className="text-sm">
                        <Link href={item.link} className="hover:underline">
                          {item.page}
                        </Link>
                      </li>
                    );
                  })}
                </ul>
                <div className=" col-span-2 w-full ">
                  <div className=" flex items-center justify-between  gap-8 md:gap-6">
                    <div className=" text-sm font-bold w-full">
                      CONNECT WITH US
                    </div>
                    <div className="w-full  flex  gap-4  justify-start items-center">
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
                    </div>
                  </div>
                </div>
              </div>
            </div>
          </div>

          <div className="flex h-32  w-full relative justify-end items-end px-8 mt-8 ">
            <Image
              className="w-full h-full object-contain  z-40"
              src={Group16}
              quality={100}
              alt="Sunset in the mountains"
            />
          </div>
        </div>
      </div>
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
