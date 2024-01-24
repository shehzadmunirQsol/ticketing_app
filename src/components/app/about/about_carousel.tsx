import React, { useRef } from 'react';
import Slider from 'react-slick';
import { useDispatch, useSelector } from 'react-redux';
import { RootState } from '~/store/store';

import 'slick-carousel/slick/slick.css';
import 'slick-carousel/slick/slick-theme.css';
import { Button } from '../../ui/button';
import NextImage from '~/components/ui/img';

function AboutCarousel(props: any) {
  const { lang } = useSelector((state: RootState) => state.layout)

  const slide = useRef<any>(null);

  const next = () => {
    slide?.current?.slickNext();
  };
  const previous = () => {
    slide?.current?.slickPrev();
  };

  const settings = {
    className: 'center slider variable-width ',
    dots: false,
    speed: 500,
    infinite: false,
    slidesToShow: 4,
    slidesToScroll: 3,
    arrows: false,

    responsive: [
      {
        breakpoint: 1400,
        settings: {
          slidesToShow: 3,
          slidesToScroll: 1,
          initialSlide: 0,
        },
      },
      {
        breakpoint:
          props?.breakpointScreens && props?.breakpointScreens[1] !== undefined
            ? props?.breakpointScreens[1]
            : 1100,
        settings: {
          slidesToShow:
            props?.breakpoint && props?.breakpoint[1] !== undefined
              ? props?.breakpoint[1]
              : 2,
          slidesToScroll: 1,
          initialSlide: 0,
          centerMode: false,
        },
      },

      {
        breakpoint: 639,
        settings: {
          slidesToShow:
            props?.breakpoint && props?.breakpoint[2] !== undefined
              ? props?.breakpoint[2]
              : 1.5,
          slidesToScroll: 1,
          initialSlide: 0,
          centerMode: false,
        },
      },
      {
        breakpoint: 510,
        settings: {
          slidesToShow:
            props?.breakpoint && props?.breakpoint[2] !== undefined
              ? props?.breakpoint[2]
              : 1,
          slidesToScroll: 1,
          initialSlide: 0,
          centerMode: false,
        },
      },
    ],
  };

  return (
    <div className="w-full  lg:px-14 md:px-14 px-4 ">
      <div
        className={` relative flex gap-3 flex-col md:flex-row h-28 md:h-auto py-6  items-center w-full md:justify-between mb-12  lg:mb-6 `}
      >
        <div>
          <p className="font-black text-white text-xl lg:text-4xl uppercase">
            {props?.heading}
          </p>
          <p className=" text-white text-lg lg:text-2xl">{props?.pera}</p>
        </div>

        <div
          className={`${
            lang?.dir == 'rtl' ? ' flex-row-reverse' : ''
          }  flex gap-2 z-10 items-center justify-center `}
        >
          <Button
            variant="rounded"
            className="button prev-btn h-10 w-10 md:h-14 md:w-14"
            onClick={() => previous()}
          >
            <i className="fa-solid fa-chevron-left"></i>
          </Button>
          <Button
            variant="rounded"
            className="button next-btn h-10 w-10 md:h-14 md:w-14"
            onClick={() => next()}
          >
            <i className="fa-solid fa-chevron-right"></i>
          </Button>
        </div>
      </div>

      <div>
        <Slider ref={slide} {...settings}>
          {props?.imageCrousel?.map((item: any, index: any) => {
            const isEven = index % 2 === 0;
            return (
              <div
                key={index}
                className={`mr-6 ${
                  isEven
                    ? 'mb-8 xsm:mb-16 sm:mb-24 md:mb-36'
                    : 'mt-8 xsm:mt-16 sm:mt-24 md:mt-36'
                }`}
              >
                <div className="content">
                  <div className="content-overlay"></div>
                  <div className="w-full h-full">
                    <NextImage
                      src={item?.img}
                      alt="/"
                      className="w-full h-full object-contain"
                      width={100}
                      height={100}
                    />
                  </div>
                  <div className="founder-hover  pl-4">
                    <p className="text-3xl pb-3 font-bold ">{item.heading}</p>
                    <p className="text-2xl pb-3 ">{item.text}</p>
                  </div>
                  <div className="content-details  text-start fadeIn-bottom fadeIn-left items-start">
                    <h2 className="text-3xl text-start font-bold">
                      {item.heading}
                    </h2>
                    <h4 className="text-xl pb-3 ">{item.text}</h4>
                    {/* <h5 className="text-hover text-start font-bold">
                      {item.hoverdesc}
                    </h5> */}
                  </div>
                </div>
              </div>
            );
          })}
        </Slider>
      </div>
    </div>
  );
}

export default AboutCarousel;
