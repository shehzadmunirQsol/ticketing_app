import React, { useRef } from 'react';
import Slider from 'react-slick';

import 'slick-carousel/slick/slick.css';
import 'slick-carousel/slick/slick-theme.css';
import DataCard from '../../common/card';
import { Card } from '../../ui/card';
import ProductCard from '../../common/card';
import { Button } from '../../ui/button';
interface producctInterface {
  class?: string;
  title: string;
  center: boolean;
  slidesToShow?: number;
}
function ProductSection(props: producctInterface) {
  const slide = useRef<any>();
  const next = () => {
    slide?.current.slickNext();
  };
  const previous = () => {
    slide?.current?.slickPrev();
  };
  const settings = {
    className: 'center slider variable-width ',

    dots: false,
    infinite: true,
    speed: 500,
    slidesToShow: props?.slidesToShow,
    slidesToScroll: props?.slidesToShow,
    centerMode: props?.center,
    arrows: false,
    slidesPerRow: 1,
    responsive: [
      {
        breakpoint: 1024,
        settings: {
          slidesToShow: 2,
          slidesToScroll: 2,
        },
      },
      {
        breakpoint: 800,
        settings: {
          slidesToShow: 2,
          slidesToScroll: 2,
          initialSlide: 2,
        },
      },

      {
        breakpoint: 640,
        settings: {
          slidesToShow: 1,
          slidesToScroll: 1,
        },
      },
    ],
  };
  return (
    <div className="relative min-h-screen w-full ">
      <div className=" relative flex flex-col md:flex-row h-28 md:h-auto py-6 gap-2 items-center w-full md:justify-between mb-6">
        <div className="text-gray-200 sm:text-2xl lg:text-5xl font-black uppercase  ">
          {props?.title}
        </div>
        <div className=" md:absolute right-10 flex gap-2 items-center justify-center ">
          <Button
            variant="rounded"
            className="button prev-btn h-14 w-14"
            onClick={() => previous()}
          >
            {/* <i className="fa-solid fa-left-arrow" /> */}
            <i className="fa-solid fa-chevron-left"></i>
          </Button>
          <Button
            variant="rounded"
            className="button next-btn h-14 w-14"
            onClick={() => next()}
          >
            <i className="fa-solid fa-chevron-right"></i>
          </Button>
        </div>
      </div>
      <div className="">
        <Slider ref={slide} {...settings}>
          {['', '', '', '', '', '', '', '', '', '', '', '', '', ''].map(
            (item, index) => {
              return (
                <div key={index}>
                  <ProductCard class={props?.class} />
                </div>
              );
            },
          )}
        </Slider>
      </div>
    </div>
  );
}

export default ProductSection;
