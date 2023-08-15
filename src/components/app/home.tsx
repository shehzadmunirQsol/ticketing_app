import React, { useRef } from 'react';
import Slider from 'react-slick';

import 'slick-carousel/slick/slick.css';
import 'slick-carousel/slick/slick-theme.css';
import DataCard from '../common/card';
import { Card } from '../ui/card';
import ProductCard from '../common/card';
import { Button } from '../ui/button';

function Home() {
  const slide = useRef<any>();
  const next = () => {
    slide?.current.slickNext();
  };
  const previous = () => {
    slide?.current?.slickPrev();
  };
  const settings = {
    dots: false,
    infinite: false,
    speed: 500,
    slidesToShow: 3,
    slidesToScroll: 1,
    arrows: false,
  };
  return (
    <div className="relative min-h-screen w-screen px-6 py-2 ">
      <div
        style={{ textAlign: 'center' }}
        className=" flex py-4 gap-2 items-center justify-between"
      >
        <div className="text-gray-200 text-5xl font-black uppercase leading-[48px]">
          Ending Soon Competitions
        </div>
        <div className=" flex gap-2 items-center justify-center">
          <Button
            variant="rounded"
            className="button prev-btn h-10 w-10"
            onClick={() => previous()}
          >
            {/* <i className="fa-solid fa-left-arrow" /> */}
            <i className="fa-solid fa-arrow-left "></i>
          </Button>
          <Button
            variant="rounded"
            className="button next-btn h-10 w-10"
            onClick={() => next()}
          >
            <i className="fa-solid fa-arrow-right"></i>
          </Button>
        </div>
      </div>
      <div>
        <Slider ref={slide} {...settings}>
          <div>
            <ProductCard class={'max-w-md'} />
          </div>
          <div>
            <ProductCard class={'max-w-md'} />
          </div>
          <div>
            <ProductCard class={'max-w-md'} />
          </div>
          <div>
            <ProductCard class={'max-w-md'} />
          </div>
          <div>
            <ProductCard class={'max-w-md'} />
          </div>
        </Slider>
      </div>
    </div>
  );
}

export default Home;
