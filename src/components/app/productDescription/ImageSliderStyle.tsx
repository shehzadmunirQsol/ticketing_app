import React, { useEffect, useState } from 'react';
import BottleImage from '~/public/assets/bottle.png';
import Image from 'next/image';
import { Button } from '~/components/ui/button';
import { useSelector } from 'react-redux';
import { RootState } from '~/store/store';
import CarImage from '../../../public/assets/card_image.png';
import CarImage1 from '../../../public/assets/car_image.png';
import CarImage2 from '../../../public/assets/ferrari.png';
import { renderNFTImage } from '~/utils/helper';

const BannerSlider = ({data}) => {

  const [currentIndex, setCurrentIndex] = useState(0);
  const [showElement, setShowElement] = useState(false);
  console.log(currentIndex,"currentIndexcurrentIndexcurrentIndex")

  // const data: any = [
  //   {
  //     image: CarImage,
  //   },
  //   {
  //     image: CarImage1,
  //   },
  //   {
  //     image: CarImage2,
  //   },
  // ];


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
    <div className="relative">
      <div className="flex items-center">
        <Button
          variant="rounded"
          className="button prev-btn h-10 w-10 md:h-14 md:w-14 absolute top-58 left-0  z-40"
          onClick={() => setCurrentIndex(currentIndex - 1)}
          disabled={currentIndex === 0}
        >
          <i className="fa-solid fa-chevron-left"></i>
        </Button>
        <div className="relative h-[18rem] lg:h-[38rem]  w-full lg:px-6 md:px-6 ">
          <Image
            alt="feature"
            src={renderNFTImage(data?.EventImages[currentIndex])}
            width={5000}
            height={5000}
            className=" object-contain object-center h-full w-full "
          />
        </div>
        <div className="absolute lg:bottom-36 md:bottom-36 bottom-28  lg:right-10 md:right-10 right-4 rounded-full w-14 p-1 bg-gradient-to-b from-primary to-neutral-900">
          <Image
            className="w-14 h-12  lg:w-full lg:h-full object-cover  rounded-full bg-white"
            src={BottleImage}
            alt="Sunset in the mountains"
          />
        </div>
        <Button
          variant="rounded"
          className="button next-btn h-10 w-10 md:h-14 md:w-14 absolute top-58 right-0  z-40"
          onClick={() => setCurrentIndex(currentIndex + 1)}
          disabled={data?.EventImages?.length - 1 === currentIndex}
        >
          <i className="fa-solid fa-chevron-right"></i>
        </Button>
      </div>
      <div className="flex flex-row gap-2 mt-4 lg:px-6 md:px-6 ">
        {data?.EventImages?.map((item, i) => {
          console.log(item,"HKSHSSHHSSHA")
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
