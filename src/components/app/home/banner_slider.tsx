import React, { useEffect, useState } from 'react';
import Background from '../../../public/assets/win-banner-1_cleanup.png';
import BgImage1 from '../../../public/assets/win-banner-2.jpeg';
import BgImage2 from '../../../public/assets/Ford-Mustang-Yellow-PNG.png';
import BgImage3 from '../../../public/assets/Ford-Mustang-PNG-Pic.png';
import BgImage4 from '../../../public/assets/ferrari.png';
import Slant from '../../../public/assets/slants.png';
import Image from 'next/image';
import { Button } from '~/components/ui/button';

const BannerSlider = () => {
  const [select, setSelect] = useState(2);

  const [currentIndex, setCurrentIndex] = useState(0);
  const [showElement, setShowElement] = useState(false);

  const carSlider: any = [
    {
      image: BgImage1,
      text: 'TOYOTA RAV4 2023',
      BannerTitle: 'WIN THIS TOYOTA',
      BannerPrice: '1000 AED',
      BannerPara: 'Get your dream car at a fraction of the price!',
      BannerDate: 'WIN SUNDAY 8 PM',
    },
    {
      image: BgImage2,
      text: 'TOYOTA RAV4 2023',
      BannerTitle: 'WIN THIS TOYOTA',
      BannerPrice: '1000 AED',
      BannerPara: 'Get your dream car at a fraction of the price!',
      BannerDate: 'WIN SUNDAY 8 PM',
    },
    {
      image: BgImage3,
      text: '2021 BMW X5 40D & £1000',
      BannerTitle: 'WIN THIS BMW',
      BannerPrice: '1000 AED',
      BannerPara: 'Get your dream car at a fraction of the price!',
      BannerDate: 'WIN SUNDAY 8 PM',
    },
    {
      image: BgImage4,
      text: 'FERARRI LAFERRARI 2014',
      BannerTitle: 'WIN THIS FERRARI',
      BannerPrice: '1000 AED',
      BannerPara: 'Get your dream car at a fraction of the price!',
      BannerDate: 'WIN SUNDAY 8 PM',
    },
  ];

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

  return (
    <div
      className="relative w-full h-screen sm:overflow-hidden"
      style={{
        background: `url(${Background.src}) no-repeat`,
        backgroundSize: '100vw 100vh',
        zIndex: 1,
      }}
    >
      <div className="relative ">
        <Image
          src={Slant}
          alt="/"
          className="absolute right-0 top-0 bg-blend-darken"
          width={500}
          height={1200}
        />
      </div>

      <div className="hidden md:block relative bg-blend-darken">
        <Image
          src={Slant}
          alt="/"
          className="absolute left-10 mt-56 transform -scale-100 "
          width={500}
          height={1200}
        />
      </div>

      <div className=" w-full  flex flex-col sm:!flex-row justify-between z-20">
        {/* text content */}

        <div
          className={`relative ${
            showElement ? 'fading-animation' : ''
          } transition-all  duration-500  ease-in-out top-32 sm:top-48 h-fit  sm:ml-20 text-white sm:max-w-[700px]`}
        >
          <p className="text-5xl font-[900] tracking-[-2px] ">
            {carSlider[currentIndex]?.BannerTitle}
          </p>
          {carSlider[currentIndex]?.BannerPrice ? (
            <p className="text-5xl tracking-[-2px] my-3 ">
              + {carSlider[currentIndex]?.BannerPrice} CASH
            </p>
          ) : (
            ''
          )}
          <p className="text-xl  font-normal ">
            {carSlider[currentIndex]?.BannerPara}
          </p>
          <p className="text-3xl tracking-[-2px] font-[900]  my-3">
            {carSlider[currentIndex]?.BannerDate}
          </p>
          <Button
            className="text-black font-sans font-[900]  tracking-[-1px]"
            variant="clip"
          >
            ENTER NOW
          </Button>
        </div>

        {/* text select cards */}
        <div className="  absolute  mb-10 right-0 top-36   z-20 w-full max-w-[700px] h-full max-h-[340px]">
          <Image
            className="drop-shadow-2xl   object-cover"
            src={carSlider[currentIndex]?.image}
            alt="/"
            fill
            // width={750}
            // height={400}
            // sizes="(max-width: 768px) 900px 500px, (max-width: 1200px) 700px 400px"
          />
        </div>
        <div className="relative w-fit right-16 z-30 sm:top-[360px] items-end h-fit flex justify-between  gap-3 mx-auto sm:mx-0">
          {carSlider.map((item: any, i: number) => (
            <div
              className="group relative top-32 max-w-[120px] text-center font-semibold hover:cursor-pointer"
              onClick={() => goToSlide(i)}
            >
              <div className="relative w-[100px] h-[60px] mx-auto border-2  border-transparent group-hover:border-primary">
                <Image
                  src={item.image}
                  alt="/"
                  fill
                  className="rounded-md object-contain group-hover:rounded-none"
                />
              </div>
              <p className="mt-3">{item.text}</p>
            </div>
          ))}
        </div>
      </div>
    </div>
  );
};

export default BannerSlider;
