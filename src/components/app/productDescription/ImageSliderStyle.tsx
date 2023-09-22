import React, { useEffect, useState } from 'react';
import BottleImage from '~/public/assets/bottle.png';
import placeHolderImage from '~/public/assets/placeholder.png';
import Image from 'next/image';
import { Button } from '~/components/ui/button';
import { renderNFTImage } from '~/utils/helper';

const BannerSlider = ({ data }: any) => {
  const [currentIndex, setCurrentIndex] = useState(0);
  const [showElement, setShowElement] = useState(false);
  console.log(currentIndex, 'currentIndexcurrentIndexcurrentIndex');

  // FOR ANIMATION IN THE
  const animateSlideChange = () => {
    setShowElement(false);
    setTimeout(() => {
      setShowElement(true);
    }, 50);
  };

  const goToSlide = (slideIndex: any) => {
    animateSlideChange(); // Show the new content with the animation
    setCurrentIndex(slideIndex);
    console.log(slideIndex, 'slideIndex');
  };

  const nextSlide = () => {
    animateSlideChange(); // Show the new content with the animation
    setCurrentIndex((prevIndex) => (prevIndex + 1) % data.length);
  };

  // 1.useEffect to change the current index
  // useEffect(() => {
  //   const interval = setInterval(() => {
  //     nextSlide();
  //   }, 4000);

  //   return function () {
  //     clearTimeout(interval);
  //   };
  // });

  // 1.useEffect for handling animation
  useEffect(() => {
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

  let eventImages: any = [];

  if (data?.EventImages?.length) {
    eventImages = [{ thumb: data?.thumb }, ...data?.EventImages];
  }

  return (
    <div className="relative">
      <div
        className={`flex items-center  ease-in animate-in  transition-transform duration-500 transform translate-x-[calc(-100% * var(${currentIndex}))]`}
      >
        <Button
          variant="rounded"
          className="button prev-btn h-10 w-10 md:h-14 md:w-14 absolute top-58 left-0  z-40"
          onClick={() => setCurrentIndex(currentIndex - 1)}
          disabled={currentIndex === 0}
        >
          <i className="fa-solid fa-chevron-left"></i>
        </Button>
        <div
          className={`relative h-[18rem] lg:h-[38rem]  w-full lg:px-6 md:px-6  ease-in animate-in  transition-transform duration-500 transform translate-x-[calc(-100% * var(${currentIndex}))]`}
          id="default-carousel"
          data-carousel="slide"
        >
          {data?.EventImages?.length ? (
            <Image
              alt="feature"
              src={renderNFTImage(eventImages[currentIndex])}
              width={5000}
              height={5000}
              loading="lazy"
              className=" object-cover rounded-sm object-center h-full w-full  duration-700 ease-in-out"
            />
          ) : (
            <Image
              alt="feature"
              src={placeHolderImage}
              width={5000}
              height={5000}
              loading="lazy"
              className=" object-cover rounded-sm object-center h-full w-full  transition-all ease-in-out animate-slide-in  duration-500"
            />
          )}
          <div className="absolute bottom-0 md:bottom-8  lg:right-10 md:right-10 right-4 rounded-full w-14 p-1 bg-gradient-to-b from-primary to-neutral-900">
            <Image
              className="w-14 h-12  lg:w-full lg:h-full object-cover  rounded-full bg-white"
              src={BottleImage}
              alt="Sunset in the mountains"
            />
          </div>
        </div>
        <Button
          variant="rounded"
          className="button next-btn h-10 w-10 md:h-14 md:w-14 absolute top-58 right-0  z-40"
          onClick={() => setCurrentIndex(currentIndex + 1)}
          disabled={eventImages?.length - 1 === currentIndex}
        >
          <i className="fa-solid fa-chevron-right"></i>
        </Button>
      </div>
      <div className="flex flex-row flex-wrap gap-2 mt-4 lg:px-6 md:px-6  ">
        {eventImages?.map((item: any, i: any) => {
          return (
            <div
              className="flex flex-row gap-2"
              key={i}
              onClick={() => goToSlide(i)}
            >
              <Image
                alt="feature"
                className="h-20 w-24"
                width={100}
                height={100}
                src={renderNFTImage(item)}
              />
            </div>
          );
        })}
      </div>
    </div>
  );
};

export default BannerSlider;
