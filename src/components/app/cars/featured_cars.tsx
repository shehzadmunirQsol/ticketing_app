import React, { useRef, useState } from 'react';
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
import { trpc } from '~/utils/trpc';
import { renderNFTImage } from '~/utils/helper';

const FeaturedCars = () => {
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

  const { lang } = useSelector((state: RootState) => state.layout);
  const [products, setProducts] = useState<Array<any>>([]);

  const [filters, setFilters] = useState({
    lang_id: lang.lang_id,
    first: 0,
    rows: 9,
    is_featured: 1,
  });

  const {
    data: prductsList,
    isFetched,
    isLoading,
    isError,
  } = trpc.event.getFeatured.useQuery(filters, {
    refetchOnWindowFocus: false,
  });

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


  return (
    <div className="hidden slg:flex sm:flex-col">
      <div className="flex max-h-[100%]  xl:max-h-[510px]">
        <div className="w-full !h-full sm:max-w-[400px]  md:max-w-[750px]  xl:max-w-[1000px] 2xl:max-w-[1200px]   relative">
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
          <div className=" ">
            <Slider {...settings} ref={slide}>
              {prductsList?.data[0]?.EventImages.map((item, i) => (
                <div className="relative md:w-[700px] md:h-[460px] lg:h-[510px] " key={i}>
                  <Image
                    src={renderNFTImage(item)}
                    alt="/"
                    // width={1300}
                    // height={500}
                    fill
                    quality={100}
                    className="object-cover h-full"
                  />
                </div>
              ))}
            </Slider>
          </div>
        </div>


        {/* product cards */}
        <div className="w-full  max-w-2xl">
          <ProductCard
            class={`w-full h-full max-w-md lg:max-w-2xl`}
            dir={`${lang?.dir}`}
            data={prductsList?.data[0]}
          />
        </div>
      </div>


      <div className="flex h-full max-h-[100%]  ">
        {/* product cards */}
        <div className="w-full max-w-2xl">
          <ProductCard
            class={`w-full h-full max-w-sm lg:max-w-2xl`}
            dir={`${lang?.dir}`}
            data={prductsList?.data[1]}
          />
        </div>


        <div className="w-full !h-full  sm:max-w-[400px]  md:max-w-[750px]  xl:max-w-[1000px] 2xl:max-w-[1200px]   relative">
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
          <div className=" h-full max-h-[520px] overflow-hidden">
            <Slider {...settings} ref={slide2}>
              {prductsList?.data[1]?.EventImages.map((item, i) => (
                <div className="relative md:w-[700px] md:h-[460px] lg:h-[520px] " key={i}>
                  <Image
                    src={renderNFTImage(item)}
                    alt="/"
                    fill
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
