import React, { useRef } from 'react';
import Slider from 'react-slick';
import 'slick-carousel/slick/slick.css';
import 'slick-carousel/slick/slick-theme.css';
import { Button } from '~/components/ui/button';
import ProductCard from '../../common/card';
import { useSelector } from 'react-redux';
import { RootState } from '~/store/store';

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

  return (
    <div className="hidden sm:flex">
      <div className="w-[800px] h-[400px] relative">
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
        <Slider {...settings} ref={slide}>
          <div className="w-[800px] h-[400px] gap-2 bg-red-400">
            <h3>Card</h3>
          </div>
          <div className="w-[800px] h-[400px] gap-2 bg-green-400">
            <h3>Card</h3>
          </div>
          <div className="w-[800px] h-[400px] gap-2 bg-blue-400">
            <h3>Card</h3>
          </div>
          <div className="w-[800px] h-[400px] gap-2 bg-yellow-400">
            <h3>Card</h3>
          </div>
        </Slider>
      </div>
      <div className="w-full">
        <ProductCard class={`w-full h-fit `} dir={`${lang?.dir}`}/>
      </div>
    </div>
  );
};

export default FeaturedCars;
