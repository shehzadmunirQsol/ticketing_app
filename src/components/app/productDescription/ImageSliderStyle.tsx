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



const ImageSliderStyle = ({ data }: any) => {

  let eventImages: any = [];

  if (data?.EventImages?.length) {
    eventImages = [{ thumb: data?.thumb }, ...data?.EventImages];
  }
 
  const slidermain = useRef<any>(null);
  const sliderthumb = useRef<any>(null);
 

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
          arrows: true
        }
      }
    ]
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
        }
      }
    ]
  };


  return (
    <div>

      <div className="relative mobmb-1">
        {
          data && data.EventImages ?
            <Slider {...productslide} className="productslider" ref={slidermain}>
              {
                data.EventImages.map((edata:any, key:any) => {
                  return <div className="item">
                    <div className="imgbx">
                      <Image
                        src={renderNFTImage(edata)}
                        alt="banner image"
                        width={100}
                        height={100}
                      />
                    </div>
                  </div>
                })

              }
            </Slider>
            :
            null
        }
        <div className="bottlebx">
          <Image
            src={BottleImage}
            alt="Sunset in the mountains"
          />
        </div>
      </div>

      <div className="relative hidden lg:block">
        {
          data && data.EventImages ?
            <Slider {...thumbslide} className="thumbslider smallarrows" ref={sliderthumb}>
              {
                data.EventImages.map((edata:any, key:any) => {
                  return (
                    <div className="item" key={key} onClick={() => slidermain.current.slickGoTo(key)}>
                      <div className="imgbx">
                        <Image
                          src={renderNFTImage(edata)}
                          alt="banner image"
                          width={100}
                          height={100}
                        />
                      </div>
                    </div>
                  )
                })

              }
            </Slider>
            :
            null
        }


      </div>

    </div>
  );
};

export default ImageSliderStyle;
