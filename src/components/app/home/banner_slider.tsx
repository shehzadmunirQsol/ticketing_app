import React, { useEffect, useState } from 'react';
import Image from 'next/image';
import { Button } from '~/components/ui/button';
import { useSelector } from 'react-redux';
import { RootState } from '~/store/store';
import { trpc } from '~/utils/trpc';
import { renderNFTImage } from '~/utils/helper';
import Link from 'next/link';
import { router } from '~/server/trpc';
import { useRouter } from 'next/router';
import langContent from '~/locales';

function BannerSlider() {
  const { lang } = useSelector((state: RootState) => state.layout);
  const [currentIndex, setCurrentIndex] = useState(0);
  const [carSlider, setCarSlider] = useState<Array<any>>([]);
  const [showElement, setShowElement] = useState(false);
  const router = useRouter();

  const initialOrderFilters: any = {
    lang_id: lang.lang_id,
    group: 'BANNER',
    is_enabled: true,
    rows: 4,
    first: 0,
    page: 0,
  };

  const { data: BannerApiData, isSuccess } = trpc.settings.get_banner.useQuery(
    initialOrderFilters,
    {
      refetchOnWindowFocus: false,
      // enabled: user?.id ? true : false,
    },
  );

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

  // // 1.useEffect to change the current index
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
      className={`mdx:py-10 lg:py-20 transition-all ease-in-out ${
        lang.dir === 'ltr' ? 'banner_img' : 'banner_img_flip'
      }`}
    >
      {isSuccess && carSlider?.length ? (
        <>
          <div className="mt-24 md:mt-32 flex flex-col lg:flex-row px-2 md:px-12 h-full">
            {/* text content */}

            <div
              className={` md:self-start md:flex-1  ${
                showElement ? 'fading-animation' : ''
              } transition-all  duration-500 ease-in-out items-center text-white z-40`}
            >
              <p className="px-4 text-2xl  sm:text-4xl md:text-5xl lg:text-[calc(3vw+10px)] xl:text-[55px] font-[900] tracking-[-1px] ">
                {carSlider[currentIndex]?.title}
              </p>
              {carSlider[currentIndex]?.price ? (
                <p className="px-4 text-2xl sm:text-4xl md:text-5xl lg:text-[calc(3vw+10px)] xl:text-[55px] tracking-[-1px] py-2 md:py-1 ">
                  + {carSlider[currentIndex]?.price}
                </p>
              ) : (
                ''
              )}
              <p className="sm:block px-4 text-xl  font-normal ">
                {carSlider[currentIndex]?.description}
              </p>
              <p className="px-4 text-lg  sm:text-2xl tracking-[-1px] font-[700]  my-3">
                {carSlider[currentIndex]?.date}
              </p>
              <>
                <Button
                  className=" mb-2 lg:mb-0 mx-4 min-w-fit sm:w-44 text-black font-sans font-[900]  tracking-[-1px]"
                  variant="clip"
                  onClick={() => {
                    router.push(carSlider[currentIndex]?.link);
                  }}
                >
                  {langContent[lang.lang].Index.banner.ENTER_BTN}
                </Button>
              </>
            </div>

            {/* text select cards */}
            <Image
              className={`py-4 lg:p-0 w-10/12 lg:w-1/2 self-center object-contain object-bottom transform rtl:-scale-x-100 ltr:scale-100 ltr:right-6 rtl:left-4 md:ltr:-right-40 md:rtl:-left-16                
                ${
                  showElement ? 'fading-animation' : ''
                } transition-all duration-500 ease-in-out items-end
                
                `}
              src={renderNFTImage(carSlider[currentIndex])}
              alt="banner image"
              quality={100}
              width={750}
              height={800}
            />
          </div>
          <div className="banner-bottom">
            <div className="hidden z-30  items-end lg:flex justify-between gap-3 mx-auto sm:mx-0">
              {carSlider.map((item: any, i: number) => (
                <div
                  key={i}
                  className="group w-[120px] text-center font-semibold hover:cursor-pointer"
                  onClick={() => goToSlide(i)}
                >
                  <div
                    className={`border-2 p-3 ${
                      currentIndex === i
                        ? 'border-primary'
                        : 'border-transparent'
                    } group-hover:border-primary`}
                  >
                    <Image
                      src={renderNFTImage(item)}
                      alt="/"
                      width={100}
                      height={100}
                      className="rounded-md object-contain object-center group-hover:rounded-none transform rtl:-scale-x-100 ltr:scale-100"
                    />
                  </div>
                  <p className="mt-1 text-xs mx-auto">{item.model}</p>
                </div>
              ))}
            </div>
          </div>
        </>
      ) : (
        ''
      )}
    </div>
  );
}

export default BannerSlider;
