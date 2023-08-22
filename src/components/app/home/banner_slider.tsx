import React, { useEffect, useState } from 'react';
import Background from '../../../public/assets/win-banner-1_cleanup.png';

import BgImage1 from '../../../public/assets/rolls.png';
import BgImage2 from '../../../public/assets/Ford-Mustang-Yellow-PNG.png';
import BgImage3 from '../../../public/assets/Ford-Mustang-PNG-File.png';
// import BgImage4 from '../../../public/assets/Ford-Mustang-PNG-Pic.png';
import Slant from '../../../public/assets/slants.png';
import Image from 'next/image';
import { Button } from '~/components/ui/button';
import { useSelector } from 'react-redux';
import { RootState } from '~/store/store';
import { trpc } from '~/utils/trpc';
import { renderNFTImage } from '~/utils/helper';

const BannerSlider = () => {
  const { lang } = useSelector((state: RootState) => state.layout);
  const [select, setSelect] = useState(2);

  const [currentIndex, setCurrentIndex] = useState(0);
  const [carSlider, setCarSlider] = useState([]);
  const [showElement, setShowElement] = useState(false);

  const initialOrderFilters: any = {
    lang_id: 1,
    group: 'BANNER',
    is_enabled: true,
    rows: 10,
    first: 0,
    page: 0,
  };

  const {
    data: BannerApiData,
    refetch: BannerRefetch,
    isFetched,
    isLoading,
    isError,
  } = trpc.settings.get_banner.useQuery(initialOrderFilters, {
    refetchOnWindowFocus: false,
    onSuccess: () => {
      setCarSlider(BannerApiData);
    },

    // enabled: user?.id ? true : false,
  });

  // console.log({ BannerApiData }, 'BannerApiData');

  // let carSlider: any = [];
  // BannerApiData?.map((item) => {
  //   const jsonPayload = JSON.parse(item.value);
  //   carSlider.push(jsonPayload);
  // });

  console.log(carSlider, 'payload');

  // const carSlider: any = [
  //   {
  //     image: BgImage1,
  //     text: 'ROLLS ROYCE PHANTOM 2023',
  //     BannerTitle: 'WIN THIS ROLLS ROYCE',
  //     BannerPrice: '1000 AED',
  //     BannerPara: 'Get your dream car at a fraction of the price!',
  //     BannerDate: 'WIN SUNDAY 8 PM',
  //   },
  //   {
  //     image: BgImage2,
  //     text: 'FORD MUSTANG 2023',
  //     BannerTitle: 'WIN THIS FORD MUSTANG',
  //     BannerPrice: '1000 AED',
  //     BannerPara: 'Get your dream car at a fraction of the price!',
  //     BannerDate: 'WIN SUNDAY 8 PM',
  //   },
  //   {
  //     image: BgImage3,
  //     text: 'FERARRI LAFERRARI 2014',
  //     BannerTitle: 'WIN THIS FERRARI',
  //     BannerPrice: '1000 AED',
  //     BannerPara: 'Get your dream car at a fraction of the price!',
  //     BannerDate: 'WIN SUNDAY 8 PM',
  //   },
  //   // {
  //   //   image: BgImage4,
  //   //   text: '2021 BMW X5 40D & £1000',
  //   //   BannerTitle: 'WIN THIS BMW',
  //   //   BannerPrice: '1000 AED',
  //   //   BannerPara: 'Get your dream car at a fraction of the price!',
  //   //   BannerDate: 'WIN SUNDAY 8 PM',

  //   // },
  // ];

  // console.log({carSliderApi},"carSliderApi")

  // FOR ANIMATION IN THE
  const animateSlideChange = () => {
    setShowElement(false); // Hide the content with the animation
    setTimeout(() => {
      setShowElement(true); // Show the new content with the animation
    }, 50); // A small delay to ensure the transition classes are applied smoothly
  };

  const goToSlide = (slideIndex: any) => {
    animateSlideChange(); // Show the new content with the animation
    setCurrentIndex(slideIndex);
  };

  const nextSlide = () => {
    animateSlideChange(); // Show the new content with the animation
    setCurrentIndex((prevIndex) => (prevIndex + 1) % carSlider.length);
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
  // console.log(carSlider[currentIndex], 'carSlider[currentIndex]');

  return (
    <div
      className={`relative w-full h-screen overflow-hidden ${
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

      <div className=" w-full mt-52 sm:mt-56 md:mt-0 flex flex-col-reverse  md:!flex-row justify-between z-20">
        {/* text content */}

        <div
          className={`relative ${
            showElement ? 'fading-animation' : ''
          } transition-all  duration-500 items-center  ease-in-out top-32 sm:top-48 h-fit mx-auto  ltr:md:ml-20 rtl:md:mr-20 text-white sm:max-w-[500px] lg:max-w-[700px]`}
        >
          <p className="px-4 text-3xl  md:text-4xl xl:text-5xl font-[900] tracking-[-2px] ">
            {carSlider[currentIndex]?.title}
          </p>
          {carSlider[currentIndex]?.price ? (
            <p className="px-4 text-2xl md:text-4xl xl:text-5xl tracking-[-2px] my-3 ">
              + {carSlider[currentIndex]?.price}
            </p>
          ) : (
            ''
          )}
          <p className="hidden sm:block px-4 text-xl  font-normal ">
            {carSlider[currentIndex]?.description}
          </p>
          <p className="px-4 text-3xl tracking-[-2px] font-[900]  my-3">
            {carSlider[currentIndex]?.date}
          </p>
          <Button
            className="mx-4 text-black font-sans font-[900]  tracking-[-1px]"
            variant="clip"
          >
            ENTER NOW
          </Button>
          <div className="block sm:hidden mx-auto  w-fit h-fit">
            <i className="fas fa-arrow-down animate-bounce text-teal-400 text-5xl my-14  "></i>
          </div>
        </div>

        {/* text select cards */}
        <div className="  absolute  top-[100px]  sm:mb-8 m-auto    md:top-[260px] lg:top-[240px] xl:top-[170px] ltr:right-6 rtl:left-6 md:ltr:right-2 md:rtl:left-4  z-20  w-[320px] h-[200px]  sm:max-w-[440px] sm:w-full sm:h-full  sm:max-h-[300px] md:max-w-[500px] md:max-h-[260px] lg:max-w-[680px] xl:max-w-[680px]  xl:max-h-[340px]">
          <Image
            className="    object-contain md:object-cover transform rtl:-scale-x-100 ltr:scale-100"
            src={renderNFTImage(carSlider[currentIndex])}
            alt="/"
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
              <p className="mt-3">{item.model}</p>
            </div>
          ))}
        </div>
      </div>
    </div>
  );
};

export default BannerSlider;