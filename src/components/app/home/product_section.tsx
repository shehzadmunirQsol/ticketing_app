import React, { useEffect, useRef, useState } from 'react';
import Slider from 'react-slick';

import 'slick-carousel/slick/slick.css';
import 'slick-carousel/slick/slick-theme.css';
import ProductCard from '../../common/card';
import { Button } from '../../ui/button';
import { useSelector } from 'react-redux';
import { RootState } from '~/store/store';
import { trpc } from '~/utils/trpc';
interface productInterface {
  class?: string;
  title: string;
  center: boolean;
  data?: any;
  slidesToShow?: number;
  type: string;
  breakpointScreens?: Array<number>;
  breakpoint?: Array<number>;
  // slide: React.Ref<null>;
}
function ProductSection(props: productInterface) {
  const { lang } = useSelector((state: RootState) => state.layout);
  const [products, setProducts] = useState<Array<any>>([]);

  const orderFilters = {
    lang_id: lang.lang_id,
  };

  const [filters, setFilters] = useState({
    first: 0,
    rows: 9,
    type: props?.type,
  });

  const { data: productsList } = trpc.event.getUpcoming.useQuery(
    { ...orderFilters, ...filters },
    {
      refetchOnWindowFocus: false,
    },
  );

  function nextPage() {
    if (products.length % filters.rows === 0) {
      setFilters({ ...filters, first: 1 + filters.first });
    }
  }
  const slide = useRef<any>(null);

  useEffect(() => {
    if (filters.first > 0 && productsList?.data?.length) {
      setProducts([...products, ...productsList.data]);
    } else if (productsList?.data?.length) {
      setProducts(productsList?.data);
    }
  }, [productsList]);

  useEffect(() => {
    setProducts([]);
    setFilters({
      first: 0,
      rows: 9,
      type: props?.type,
    });
  }, [lang.lang_id]);

  const next = () => {
    slide?.current?.slickNext();
  };
  const previous = () => {
    slide?.current?.slickPrev();
  };
  const settings = {
    className: 'center slider variable-width flex gap-3',

    dots: false,
    infinite: false,
    speed: 500,
    slidesToShow:
      props?.slidesToShow && products.length > props?.slidesToShow
        ? props?.slidesToShow
        : products.length,
    slidesToScroll: 1,
    arrows: false,
    centerMode: false,
    mobileFirst: true,
    responsive: [
      {
        breakpoint:
          props?.breakpointScreens && props?.breakpointScreens[0] !== undefined
            ? props?.breakpointScreens[0]
            : 1024,
        settings: {
          slidesToShow:
            props?.breakpoint && props?.breakpoint[0] !== undefined
              ? props?.breakpoint[0]
              : 2,
          slidesToScroll: 1,
          centerMode: false,
        },
      },
      {
        breakpoint:
          props?.breakpointScreens && props?.breakpointScreens[1] !== undefined
            ? props?.breakpointScreens[1]
            : 800,
        settings: {
          slidesToShow:
            props?.breakpoint && props?.breakpoint[1] !== undefined
              ? props?.breakpoint[1]
              : 2,
          slidesToScroll: 1,
          centerMode: false,
        },
      },

      {
        breakpoint:
          props?.breakpointScreens && props?.breakpointScreens[2] !== undefined
            ? props?.breakpointScreens[2]
            : 600,
        settings: {
          slidesToShow:
            props?.breakpoint && props?.breakpoint[2] !== undefined
              ? props?.breakpoint[2]
              : 1,
          slidesToScroll: 1,
        },
      },
      {
        breakpoint: 480,
        settings: {
          slidesToShow: 1.05,
          slidesToScroll: 1,
        },
      },
    ],
  };
  return (
    <div className=" max-w-[1600px]  mx-auto w-full ">
      <div className="px-4 relative flex gap-3 flex-col md:flex-row h-28 md:h-auto py-6  z-30 items-center w-full md:justify-between mb-6">
        <p className="text-gray-200 !text-xl sm:!text-3xl lg:!text-5xl font-black uppercase  ">
          {props?.title}
        </p>
        <div
          className={`${
            lang?.dir == 'rtl' ? ' flex-row-reverse' : 'md:ml-0'
          }  flex gap-2 z-10 items-center justify-center `}
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

      <div className="z-30 px-4 w-full mx-auto">
        {/* glow */}
        <div className="relative">
          {props.type === 'no-glow' ? (
            ''
          ) : (
            <div
              className={`absolute bottom-10 ${
                props.type == 'closing' ? 'right-0' : 'left-0'
              }  z-2  w-1/5 h-3/5  bg-teal-400 bg-opacity-50 rounded-full blur-3xl`}
            ></div>
          )}
        </div>

        <Slider ref={slide} {...settings}>
          {products.map((item, index) => {
            return (
              <div key={index} className={`${props?.class} z-10 `}>
                <ProductCard
                  isLast={index === products.length - 1}
                  nextPage={nextPage}
                  data={item}
                  type={props.type}
                  class={
                    products.length != index + 1
                      ? 'rtl:mr-0 rtl:ml-4 ltr:mr-4 ltr:ml-0'
                      : ''
                  }
                  dir={`${lang?.dir}`}
                />
              </div>
            );
          })}
          {products.length === 0 ? (
            <div className="text-center w-full py-10 text-lg">
              Coming Soon...
            </div>
          ) : (
            ''
          )}
        </Slider>
      </div>
    </div>
  );
}

export default ProductSection;
