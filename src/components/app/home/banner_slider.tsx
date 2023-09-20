'use client';
import React, { useEffect, useState } from 'react';

import Slant from '../../../public/assets/slants.png';
import Image from 'next/image';
import { Button } from '~/components/ui/button';
import { useSelector } from 'react-redux';
import { RootState } from '~/store/store';
import { trpc } from '~/utils/trpc';
import { renderNFTImage } from '~/utils/helper';
import Link from 'next/link';

const BannerSlider = () => {
  const { lang } = useSelector((state: RootState) => state.layout);
  const [select, setSelect] = useState(2);

  const [currentIndex, setCurrentIndex] = useState(0);
  const [carSlider, setCarSlider] = useState<Array<any>>([]);
  const [showElement, setShowElement] = useState(false);

  const initialOrderFilters: any = {
    lang_id: lang.lang_id,
    group: 'BANNER',
    is_enabled: true,
    rows: 4,
    first: 0,
    page: 0,
  };

  const {
    data: BannerApiData,
    refetch: BannerRefetch,
    isFetched,
    isLoading,
    isError,
    isSuccess,
  } = trpc.settings.get_banner.useQuery(initialOrderFilters, {
    refetchOnWindowFocus: false,
    // enabled: user?.id ? true : false,
  });

  useEffect(() => {
    if (BannerApiData?.data) {
      setCarSlider(BannerApiData?.data);
    }
  }, [BannerApiData?.data]);

  // FOR ANIMATION IN THE
  const animateSlideChange = () => {
    setShowElement(false); // Hide the content with the animation
    setTimeout(() => {
      setShowElement(true); // Show the new content with the animation
    }, 1000); // A small delay to ensure the transition classes are applied smoothly
  };

  const goToSlide = (slideIndex: any) => {
    animateSlideChange(); // Show the new content with the animation
    setCurrentIndex(slideIndex);
  };

  const nextSlide = () => {
    animateSlideChange(); // Show the new content with the animation
    setCurrentIndex((prevIndex) => (prevIndex + 1) % carSlider?.length);
  };

  // useEffect for infinite loops

  // 1.useEffect to change the current index
  useEffect(() => {
    const interval = setInterval(() => {
      nextSlide();
    }, 4000);

    return function () {
      clearTimeout(interval);
    };
  });

  // 1.useEffect for handling animation
  useEffect(() => {
    // Function to handle the animation when currentIndex changes
    const animateSlideChange = () => {
      setShowElement(false); // Hide the content with the animation
      setTimeout(() => {
        setShowElement(true); // Show the new content with the animation
      }, 0); // A small delay to ensure the transition classes are applied smoothly
    };

    animateSlideChange(); // Call the animation function whenever currentIndex changes

    // Clear the animation class after the animation is completed
    const animationTimeout = setTimeout(() => {
      setShowElement(false);
    }, 3000); // Adjust the duration as needed to match your transition duration in CSS

    return () => clearTimeout(animationTimeout);
  }, [currentIndex]);

  return (
    <div
      className={`relative w-full h-[43rem] sm:h-[41rem] md:h-[40rem] lg:h-[39rem] transition-all ease-in-out overflow-hidden ${
        lang.dir === 'ltr' ? 'banner_img' : 'banner_img_flip'
      }`}
    >
      <div className="relative ">
        <Image
          src={Slant}
          alt="/"
          className="absolute ltr:right-0 rtl:left-0 top-0 bg-blend-darken transform rtl:-scale-x-100 ltr:scale-100"
          width={500}
          height={1200}
        />
      </div>
      <div className="hidden md:block relative bg-blend-darken">
        <Image
          src={Slant}
          alt="/"
          className="absolute ltr:left-10 mt-56 transform rtl:-scale-y-100 ltr:-scale-100"
          width={500}
          height={1200}
        />
      </div>
      {isSuccess && carSlider?.length ? (
      <div className=" relative w-full mx-auto  mt-52 sm:mt-56 md:mt-0 flex flex-col-reverse  lg:!flex-row justify-between z-20">
          {/* text content */}

          <div
            className={`relative ${
              showElement ? 'fading-animation' : ''
            } transition-all  duration-500 ease-in-out items-center   top-32 sm:top-48 h-fit mx-auto  ltr:md:ml-20 rtl:md:mr-20 text-white sm:max-w-[500px] lg:max-w-[700px]`}
          >
            <p className="px-4 text-2xl  sm:text-4xl md:text-5xl xl:text-[64px] font-[900] -tracking-[-2.56px] ">
              {carSlider[currentIndex]?.title}
            </p>
            {carSlider[currentIndex]?.price ? (
              <p className="px-4 text-2xl sm:text-4xl md:text-5xl xl:text-[64px] tracking-[-2.56px] py-2 md:py-6 ">
                + {carSlider[currentIndex]?.price}
              </p>
            ) : (
              ''
            )}
            <p className="sm:block px-4 text-xl  font-normal ">
              {carSlider[currentIndex]?.description}
            </p>
            <p className="px-4 text-lg  sm:text-2xl tracking-[-2px] font-[700]  my-3">
              {carSlider[currentIndex]?.date}
            </p>
            <Link href="/cars">
              <Button
                className="mx-4 text-black font-sans font-[900]  tracking-[-1px]"
                variant="clip"
              >
                {lang.lang_id === 1 ? 'ENTER NOW' : 'أدخل الأن'}
              </Button>
            </Link>
          </div>

          {/* text select cards */}
          <div
            className={`  absolute   ${
              showElement ? 'fading-animation' : ''
            } transition-all  duration-500 ease-in-out   sm:mb-8 m-auto -bottom-64  xl:-bottom-64 ltr:-right-6 rtl:-left-6 md:ltr:-right-32 md:rtl:-left-32  z-20  w-[320px] h-[200px]  sm:max-w-[440px] sm:w-full sm:h-full  sm:max-h-[300px] md:max-w-[500px] md:max-h-[260px] lg:max-w-[680px] xl:max-w-[780px] xl:max-h-[580px]   `}
          >
            <Image
              className="    object-contain  transform rtl:-scale-x-100 ltr:scale-100 ltr:right-6 rtl:left-4 md:ltr:-right-40 md:rtl:-left-16"
              src={renderNFTImage(carSlider[currentIndex])}
              alt="banner image"
              fill
              quality={100}

              // width={750}
              // height={400}
              // sizes="(max-width: 768px) 900px 500px, (max-width: 1200px) 700px 400px"
            />
          </div>
          <div className="relative hidden    ltr:right-16 rtl:left-16 z-30  md:top-[380px]  items-end h-fit lg:flex justify-between  gap-3 mx-auto sm:mx-0">
            {carSlider.map((item: any, i: number) => (
              <div
                key={i}
                className="group relative top-32 max-w-[120px] text-center font-semibold hover:cursor-pointer"
                onClick={() => goToSlide(i)}
              >
                <div
                  className={`relative   w-[100px] h-[60px] mx-auto border-2  ${
                    currentIndex === i ? 'border-primary' : 'border-transparent'
                  } group-hover:border-primary`}
                >
                  <Image
                    src={renderNFTImage(item)}
                    alt="/"
                    fill
                    className="rounded-md object-contain group-hover:rounded-none transform rtl:-scale-x-100 ltr:scale-100"
                  />
                </div>
                <p className="mt-1 text-xs max-w-[100px] mx-auto">
                  {item.model}
                </p>
              </div>
            ))}
          </div>
        </div>
      ) : (
        ''
      )}
    </div>
  );
};

export default BannerSlider;
