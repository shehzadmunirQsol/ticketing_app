import React, { useEffect, useRef, useState } from 'react';
import BottleImage from '~/public/assets/bottle.png';
import Image from 'next/image';
import { Button } from '~/components/ui/button';
import { renderNFTImage } from '~/utils/helper';
import Slider from 'react-slick';
import 'slick-carousel/slick/slick.css';
import 'slick-carousel/slick/slick-theme.css';

import 'photoswipe/dist/photoswipe.css';
import { Gallery, Item } from 'react-photoswipe-gallery';

function SampleNextArrow(props: any) {
  const { className, style, onClick } = props;
  return (
    <Button
      variant="rounded"
      className="slide-arrow next-arrow h-10 w-10 md:h-14 md:w-14  absolute right-2 md:right-0 top-[42%]  md:top-[38%] z-40"
      onClick={onClick}
    >
      <i className="fa-solid fa-chevron-right"></i>
    </Button>
  );
}

function SamplePrevArrow(props: any) {
  const { className, style, onClick } = props;
  return (
    <Button
      variant="rounded"
      className="slide-arrow prev-arrow h-10 w-10 md:h-14 md:w-14 absolute left-2 md:left-0 top-[42%]  md:top-[38%]  z-40"
      onClick={onClick}
    >
      <i className="fa-solid fa-chevron-left"></i>
    </Button>
  );
}

const ImageSliderStyleNew = ({ data }: any) => {
  const [currentIndex, setCurrentIndex] = useState(0);
  const [showElement, setShowElement] = useState(false);
  console.log(currentIndex, 'currentIndexcurrentIndexcurrentIndex');

  // FOR ANIMATION IN THE

  const slide = useRef<any>(null);

  let eventImages: any = [];

  if (data?.EventImages?.length) {
    eventImages = [{ thumb: data?.thumb }, ...data?.EventImages];
  }
  const settings = {
    className: ' h-full w-full md:px-6 ',
    customPaging: function (i: number) {
      return (
        <a className="h-20 w-28 cursor-pointer ">
          <Image
            alt="feature"
            src={renderNFTImage(eventImages[i + 1])}
            width={1000}
            height={1000}
            loading="lazy"
            className="h-20 w-28 object-cover rounded-sm object-center duration-700 ease-in-out"
          />
          {/* <img src={`${baseUrl}/abstract0${i + 1}.jpg`} /> */}
        </a>
      );
    },
    dots: true,

    appendDots: (dots: any) => <ul>{dots}</ul>,
    dotsClass: 'hidden md:flex flex-row flex-wrap gap-2 mt-4',

    infinite: true,
    speed: 500,
    slidesToShow: 1,
    slidesToScroll: 1,
    mobileFirst: true,
    autoplay: true,
    autoplaySpeed: 5000,
    fade: false,
    arrows: false,
    nextArrow: <SampleNextArrow />,
    prevArrow: <SamplePrevArrow />,
  };

  return (
    <div className="relative">
      <div
        className={`  ease-in animate-in  transition-transform duration-500 transform translate-x-[calc(-100% * var(${currentIndex}))]`}
        id="galleryID"
      >
        <Gallery>
          <Slider {...settings}>
            {data?.EventImages?.length &&
              data?.EventImages?.map((item: any, index: number) => {
                return (
                  <Item
                    original={renderNFTImage(item)}
                    thumbnail={renderNFTImage(item)}
                    width="1024"
                    height="576"
                    key={index}
                  >
                    {({ ref, open }) => (
                      <div
                        ref={ref as React.MutableRefObject<HTMLDivElement>}
                        onClick={open}
                        className="h-[18rem] lg:h-[28rem] relative"
                      >
                        <Image
                          alt="feature"
                          src={renderNFTImage(item)}
                          width={5000}
                          height={5000}
                          loading="lazy"
                          className="object-cover rounded-sm object-center h-full w-full  duration-700 ease-in-out"
                        />
                      </div>
                    )}
                  </Item>
                );
              })}
          </Slider>
        </Gallery>
      </div>
      <div className="bottlebx absolute lg:right-10 md:right-10 right-4 rounded-full w-14 p-1 bg-gradient-to-b from-primary to-neutral-900">
        <Image
          className="w-14 h-12  lg:w-full lg:h-full object-cover  rounded-full bg-white"
          src={BottleImage}
          alt="Sunset in the mountains"
        />
      </div>
    </div>
  );
};

export default ImageSliderStyleNew;
