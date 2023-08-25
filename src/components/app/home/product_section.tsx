import React, { useEffect, useRef, useState } from 'react';
import Slider from 'react-slick';

import 'slick-carousel/slick/slick.css';
import 'slick-carousel/slick/slick-theme.css';
import DataCard from '../../common/card';
import { Card } from '../../ui/card';
import ProductCard from '../../common/card';
import { Button } from '../../ui/button';
import { useSelector } from 'react-redux';
import { RootState } from '~/store/store';
import { trpc } from '~/utils/trpc';
interface producctInterface {
  class?: string;
  title: string;
  center: boolean;
  data?: any;
  slidesToShow?: number;
  type: string;
}
function ProductSection(props: producctInterface) {
  const { lang } = useSelector((state: RootState) => state.layout);
  const todayDate = new Date();
  const [products, setProducts] = useState<Array<any>>([]);

  const orderfilters = {
    lang_id: lang.lang_id,
  };

  const [filters, setFilters] = useState({
    first: 0,
    rows: 9,
    type: props?.type,
    category_id: 1,
  });

  const {
    data: prductsList,
    isFetched,
    refetch,
    isLoading,
    isError,
  } = trpc.event.getUpcomimg.useQuery({...orderfilters,...filters}, {
    refetchOnWindowFocus: false,
  });

  console.log({ prductsList }, 'array of somthing:', props?.type);
  console.log({ products }, 'products', props?.type);
  
  function nextPage() {
    console.log('Next page emitted');
    if (products.length % filters.rows === 0) {
      setFilters({ ...filters, first: 1 + filters.first });
    }
  }
  const slide = useRef<any>();

  useEffect(() => {
    if (filters.first > 0 && prductsList?.data?.length) {
      setProducts([...products, ...prductsList.data]);
    } else if (prductsList?.data?.length) {
      setProducts(prductsList?.data);
    }
  }, [prductsList]);

  useEffect(()=>{
    setProducts([])
  },[lang.lang_id])

  const next = () => {
    slide?.current?.slickNext();
  };
  const previous = () => {
    slide?.current?.slickPrev();
  };
  const settings = {
    className: 'center slider variable-width ',

    dots: false,
    // infinite: false,
    speed: 500,
    slidesToShow: props?.slidesToShow,
    slidesToScroll: props?.slidesToShow,
    // centerMode: false,
    arrows: false,
    // slidesPerRow: 1,
    responsive: [
      {
        breakpoint: 1024,
        settings: {
          slidesToShow: 3,
          slidesToScroll: 2,
          initialSlide: 1,
        },
      },
      {
        breakpoint: 800,
        settings: {
          slidesToShow: 2,
          slidesToScroll: 1,
          initialSlide: 1,
          centerMode: false,
        },
      },

      {
        breakpoint: 640,
        settings: {
          slidesToShow: 1,
          slidesToScroll: 1,
          centerMode: false,
        },
      },
    ],
  };
  return (
    <div className="   w-full ">
      <div className=" relative flex flex-col md:flex-row h-28 md:h-auto py-6  items-center w-full md:justify-between mb-6">
        <p className="text-gray-200 !text-xl sm:!text-3xl lg:!text-5xl font-black uppercase  ">
          {props?.title}
        </p>
        <div
          className={`${
            lang?.dir == 'rtl' ? ' flex-row-reverse' : 'md:absolute right-10'
          }  flex gap-2 items-center justify-center `}
        >
          <Button
            variant="rounded"
            className="button prev-btn h-10 w-10 md:h-14 md:w-14"
            onClick={() => previous()}
          >
            {/* <i className="fa-solid fa-left-arrow" /> */}
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

      <div className="relative z-10">
        {/* glow */}
        <div className="absolute bottom-10 right-0  z-2  w-1/5 h-3/5  bg-teal-400 bg-opacity-50 rounded-full blur-3xl"></div>

        <Slider ref={slide} {...settings}>
          {products.map((item, index) => {
            return (
              <div key={index} className="">
                <ProductCard
                  isLast={index === products.length - 1}
                  nextPage={nextPage}
                  data={item}
                  class={`${props?.class} `}
                  dir={`${lang?.dir}`}
                />
              </div>
            );
          })}
        </Slider>
      </div>
    </div>
  );
}

export default ProductSection;
