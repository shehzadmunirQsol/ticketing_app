import React, { useRef } from 'react';
import Slider from 'react-slick';
import 'slick-carousel/slick/slick.css';
import 'slick-carousel/slick/slick-theme.css';
import { Button } from '~/components/ui/button';
import ProductCard from '../../common/card';
import { useSelector } from 'react-redux';
import { RootState } from '~/store/store';

import ImageSlider from '~/public/assets/cars_gallery.png';
import ImageSlider2 from '~/public/assets/cars_gallery_1.png';
import Image from 'next/image';

const FeaturedCars = () => {
  const { lang } = useSelector((state: RootState) => state.layout);
  const settings = {
    dots: false,
    infinite: true,
    speed: 500,
    slidesToShow: 1,
    slidesToScroll: 1,
    arrows: false,
  };
  const slide = useRef<any>();
  const slide2 = useRef<any>();

  const next = () => {
    slide?.current.slickNext();
  };

  const previous = () => {
    slide?.current?.slickPrev();
  };

  const next2 = () => {
    slide2?.current.slickNext();
  };

  const previous2 = () => {
    slide2?.current?.slickPrev();
  };

  const Images = [
    {
      image: ImageSlider,
    },
    {
      image: ImageSlider2,
    },
  ];

  return (
    <div className="hidden sm:flex sm:flex-col">
      <div className="flex ">
        <div className="w-full xl:max-w-[1000px] 2xl:max-w-[1200px]  h-[510px] relative">
          {/* buttons */}
          <div className="px-4 absolute w-full h-full flex justify-between items-center my-auto z-10">
            <Button
              variant="rounded"
              className="button prev-btn h-6 w-6 md:h-10 md:w-10"
              onClick={() => previous()}
            >
              <i className="fa-solid fa-chevron-left"></i>
            </Button>
            <Button
              variant="rounded"
              className="button next-btn h-6 w-6 md:h-10 md:w-10"
              onClick={() => next()}
            >
              <i className="fa-solid fa-chevron-right"></i>
            </Button>
          </div>
          {/* slider */}
          <div className=" h-[510px] ">
            <Slider {...settings} ref={slide}>
              {Images.map((item, i) => (
                <div className="relative w-full h-full" key={i}>
                  <Image
                    src={item.image}
                    alt="/"
                    width={1300}
                    height={510}
                    quality={100}
                    className="object-cover"
                  />
                </div>
              ))}
            </Slider>
          </div>
        </div>

        <div className="w-full max-w-2xl">
          <ProductCard
            class={`w-full h-fit max-w-sm lg:max-w-2xl`}
            dir={`${lang?.dir}`}
          />
        </div>
      </div>

      <div className="flex ">
        <div className="w-full max-w-2xl">
          <ProductCard
            class={`w-full h-fit max-w-sm lg:max-w-2xl`}
            dir={`${lang?.dir}`}
          />
        </div>

        <div className="w-full xl:max-w-[1000px] 2xl:max-w-[1200px]  h-[510px] relative">
          {/* buttons */}
          <div className="px-4 absolute w-full h-full flex justify-between items-center my-auto z-10">
            <Button
              variant="rounded"
              className="button prev-btn h-6 w-6 md:h-10 md:w-10"
              onClick={() => previous2()}
            >
              <i className="fa-solid fa-chevron-left"></i>
            </Button>
            <Button
              variant="rounded"
              className="button next-btn h-6 w-6 md:h-10 md:w-10"
              onClick={() => next2()}
            >
              <i className="fa-solid fa-chevron-right"></i>
            </Button>
          </div>
          {/* slider */}
          <div className=" h-[510px] overflow-hidden">
            <Slider {...settings} ref={slide2}>
              {Images.map((item, i) => (
                <div className="relative w-full h-full" key={i}>
                  <Image
                    src={item.image}
                    alt="/"
                    width={1300}
                    height={510}
                    quality={100}
                    className="object-cover"
                  />
                </div>
              ))}
            </Slider>
          </div>
        </div>
      </div>
    </div>
  );
};

export default FeaturedCars;
