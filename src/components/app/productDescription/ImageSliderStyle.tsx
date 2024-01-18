import React, { useRef } from 'react';
import { useDispatch, useSelector } from 'react-redux';
import { RootState } from '~/store/store';
import BottleImage from '~/public/assets/bottle.png';
import NextImage from '@/ui/img';
import { renderNFTImage } from '~/utils/helper';
import Slider from 'react-slick';
import 'slick-carousel/slick/slick.css';
import 'slick-carousel/slick/slick-theme.css';
import 'photoswipe/dist/photoswipe.css';

import 'photoswipe/dist/photoswipe.css';
import { Gallery, Item } from 'react-photoswipe-gallery';


export default function ImageSliderStyle({ data }: any) {
  let eventImages: any = [];

  if (data?.EventImages?.length) {
    eventImages = [{ thumb: data?.thumb }, ...data?.EventImages];
  }

  const slidermain = useRef<any>(null);
  const sliderthumb = useRef<any>(null);
  const { lang } = useSelector((state: RootState) => state.layout);
  console.log(lang.lang);
  var checkRtl = false;
  if(lang.lang === 'ar') {
    checkRtl = true;
  }

  const productslide = {
    asNavFor: sliderthumb.current,
    ref: slidermain,
    slidesToShow: 1,
    arrows: false,
    swipeToSlide: true,
    focusOnSelect: true,
    responsive: [
      {
        breakpoint: 991,
        settings: {
          arrows: true,
        },
      },
    ],
  };

  const thumbslide = {
    asNavFor: slidermain.current,
    ref: sliderthumb,
    slidesToShow: 5,
    infinite: false,
    responsive: [
      {
        breakpoint: 767,
        settings: {
          slidesToShow: 3,
          slidesToScroll: 1,
        },
      },
    ],
  };

  return (
    <div>
      <div className="relative mobmb-1">
        {data && data.EventImages ? (
          <Gallery>
            <Slider {...productslide} className="productslider" ref={slidermain}>
            {data.EventImages.map((edata: any, key: any) => {
              return (
                <Item
                    original={renderNFTImage(edata)}
                    thumbnail={renderNFTImage(edata)}
                    width="1000"
                    height="667"
                    key={key}
                  >
                    {({ ref, open }) => (
                      <div
                        ref={ref as React.MutableRefObject<HTMLDivElement>}
                        onClick={open}
                        className="h-[18rem] lg:h-[28rem] relative"
                      >
                        <NextImage
                          alt="feature"
                          src={renderNFTImage(edata)}
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
        ) : null}
        <div className="bottlebx">
          <NextImage src={BottleImage} alt="Sunset in the mountains" />
        </div>
      </div>

      <div className="relative hidden lg:block">
        {data && data.EventImages ? (
          <Slider
            {...thumbslide}
            className="thumbslider smallarrows"
            ref={sliderthumb}
          >
            {data.EventImages.map((edata: any, key: any) => {
              return (
                <div
                  className="item"
                  key={key}
                  onClick={() => slidermain.current.slickGoTo(key)}
                >
                  <div className="imgbx">
                    <NextImage
                      src={renderNFTImage(edata)}
                      alt="banner image"
                      width={100}
                      height={100}
                    />
                  </div>
                </div>
              );
            })}
          </Slider>
        ) : null}
      </div>
    </div>
  );
}
