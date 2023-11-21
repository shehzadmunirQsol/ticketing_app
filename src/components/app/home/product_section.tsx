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
  type: 'upcoming' | 'closing' | 'drawn';
  breakpointScreens?: Array<number>;
  breakpoint?: Array<number>;
  categoryId?: 1 | 2;
  // slide: React.Ref<null>;
}
function ProductSection(props: productInterface) {
  const { lang } = useSelector((state: RootState) => state.layout);
  const [products, setProducts] = useState<Array<any>>([]);

  const [filters, setFilters] = useState({
    first: 0,
    rows: 9,
    type: props?.type,
    lang_id: lang.lang_id,
    category_id: props?.categoryId,
  });

  const { data: productsList } = trpc.event.getUpcoming.useQuery(filters, {
    refetchOnWindowFocus: false,
  });

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
      lang_id: lang.lang_id,
      category_id: props?.categoryId,
    });
  }, [lang.lang_id]);

  const previous = () => {
    console.log('previous');
    slide?.current?.slickPrev();
  };
  const next = () => {
    console.log('next');
    slide?.current?.slickNext();
  };

  return (
    <div className=" max-w-[1600px]  mx-auto w-full ">
      <div className="pt-6 relative gap-3 flex-col md:flex-row md:h-auto z-30 sm:items-center items-start w-full md:justify-between mb-3 sm:mb-6 flex h-fit">
        <p className="text-gray-200 !text-xl sm:!text-3xl lg:!text-5xl font-black uppercase ">
          {props?.title}
        </p>
        {/* <div
          className={`${
            lang?.dir == 'rtl' ? ' flex-row-reverse' : 'md:ml-0'
          } gap-2 z-10 items-center justify-center sm:flex hidden`}
        >
          <Button
            variant="rounded"
            className="button prev-btn h-10 w-10 md:h-14 md:w-14"
            onClick={previous}
          >
            <i className="fa-solid fa-chevron-left"></i>
          </Button>
          <Button
            variant="rounded"
            className="button next-btn h-10 w-10 md:h-14 md:w-14"
            onClick={next}
          >
            <i className="fa-solid fa-chevron-right"></i>
          </Button>
        </div> */}
      </div>

      <div className="z-30 w-full mx-auto">
        {/* glow */}
        <div className="relative">
          <div
            className={`absolute bottom-10 ${
              props.type == 'closing' ? 'right-0' : 'left-0'
            }  z-2  w-1/5 h-3/5  bg-teal-400 bg-opacity-50 rounded-full blur-3xl`}
          />
        </div>

        <div className="prodrow">
          {products.map((item, index) => {
            return (
              <div key={index} className="col-prod">
                <ProductCard
                  isLast={index === products.length - 1}
                  nextPage={nextPage}
                  data={item}
                  type={props.type}
                  class={
                    products.length != index + 1
                      ? ''
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
      </div>
      </div>
    </div>
  );
}

export default ProductSection;
