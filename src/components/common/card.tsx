import React, { useEffect, useRef } from 'react';
import CarImage from '~/public/assets/card_image.png';
import BottleImage from '~/public/assets/bottle.png';
import { Progress } from '../ui/progress';
import { Button } from '../ui/button';
import { URIGenerator, customTruncate, renderNFTImage } from '~/utils/helper';
import { useSelector } from 'react-redux';
import { RootState } from '~/store/store';
import Link from 'next/link';
import langContent from '~/locales';
import NextImage from '../ui/img';

interface CardInterface {
  class?: string;
  dir?: string;
  cash?: any;
  data?: any;
  nextPage?: () => void;
  isLast?: boolean;
  isCash?: boolean;
  type?: string;
}

function ProductCard(props: CardInterface) {
  const cardRef = useRef<HTMLDivElement>(null);
  const { lang } = useSelector((state: RootState) => state.layout);

  /**
   * Implement Intersection Observer to check if the last Card in the array is visible on the screen, then set a new limit
   */
  useEffect(() => {
    if (!cardRef?.current) return;

    const observer = new IntersectionObserver(([entry]: any) => {
      if (props?.isLast && entry.isIntersecting) {
        if (props?.nextPage) props?.nextPage();
        observer.unobserve(entry.target);
      }
    });

    observer.observe(cardRef.current);
  }, [props?.isLast]);

  const spaceElement = props?.isCash ? null : (
    <div className="h-8 xl:h-9 hidden md:block" />
  );
  const isLastDay =
    props?.data?.draw_date === null && props?.data?.end_date
      ? props?.data?.end_date?.getTime() <= Date.now() + 24 * 60 * 60 * 1000
      : false;

  const categoryRoute = props?.data?.category_id === 1 ? 'cars' : 'cash';

  return (
    props?.data && (
      <Link
        href={`/${categoryRoute}/${URIGenerator(
          props?.data?.EventDescription[0]?.name,
          props?.data?.id,
        )}`}
      >
        <div
          dir={props?.dir}
          className={`rounded-sm shadow-lg bg-card ${props?.class}`}
          ref={cardRef}
        >
          <div className="relative ">
            {isLastDay ? (
              <div className="font-bold absolute top-0 w-fit p-2 z-2 bg-primary text-black text-sm">
                <span className="">CLOSES TODAY</span>{' '}
                {props?.data?.end_date?.getHours()?.toString()?.length > 1
                  ? props?.data?.end_date?.getHours()
                  : '0' + props?.data?.end_date?.getHours()}
                :
                {props?.data?.end_date?.getMinutes()?.toString()?.length > 1
                  ? props?.data?.end_date?.getMinutes()
                  : '0' + props?.data?.end_date?.getMinutes()}{' '}
              </div>
            ) : (
              ''
            )}
            <NextImage
              width={550}
              height={450}
              className="w-full h-[180px] sm:h-[230px] object-cover bg-white"
              src={props?.cash ?? renderNFTImage(props?.data) ?? CarImage}
              quality={100}
              alt="Sunset in the mountains"
            />
            <div className="absolute -bottom-8 left-4 rounded-full w-20 p-1 bg-gradient-to-b from-primary to-neutral-900">
              <NextImage
                className="w-full h-full object-cover  rounded-full bg-white"
                src={BottleImage}
                alt="Sunset in the mountains"
              />
            </div>
          </div>

          <div className="px-6 mt-6 py-4">
            <div className="flex flex-col gap-1 mb-2">
              <div className="flex justify-between items-center gap-3 mb-2">
                <span className=" text-xs text-gray-300">
                  {Math.round(
                    (Number(props?.data?.tickets_sold) /
                      Number(props?.data?.total_tickets)) *
                      100,
                  )}
                  % {langContent[lang.lang].Index.productcard.SOLD_TITLE}
                </span>
                <span className="text-xs text-gray-300">
                  {(props?.data?.tickets_sold).toLocaleString()} /{' '}
                  {(props?.data?.total_tickets).toLocaleString()}
                </span>
              </div>
              <Progress
                value={
                  (Number(props?.data?.tickets_sold) /
                    Number(props?.data?.total_tickets)) *
                  100
                }
                className="w-full"
              />
            </div>
            <div className="font-bold overflow-hidden h-13 md:h-20 text-xl lg:text-2xl xl:text-3xl line-clamp-1">
              {langContent[lang.lang].Index.productcard.WIN_TITLE ?? ''}
              <span className="text-gray-200  font-semibold mx-2 ">
                {props?.data?.EventDescription[0]?.name}
              </span>
            </div>
            <div className="relative w-full opacity-75  text-gray-200 text-md text-xs md:text-base font-light sm:font-normal leading-normal">
              <p className="h-12 overflow-hidden">
                {customTruncate(props?.data?.EventDescription[0]?.desc, 100)}
              </p>
            </div>
            <hr className=" opacity-20 mt-4" />

            {props?.data?.category_id === 1 && props?.data?.cash_alt ? (
              <div className="h-15 md:h-6 overflow-hidden mt-2">
                <span className="text-gray-200 text-md text-sm xl:text-lg font-semibold leading-[18px]">
                  {langContent[lang.lang].Index.productcard.ALTERNATIVE_TITLE}
                </span>
                <br className="block md:hidden" />
                <span className="text-primary text-sm xl:text-lg font-[500] leading-[18px]">
                  {' '}
                  AED {(props?.data?.cash_alt ?? 0)?.toLocaleString()}
                </span>
              </div>
            ) : (
              spaceElement
            )}

            <div className="flex  justify-between items-center mt-4 sm:mt-8 gap-4">
              <div className="text-primary text-xl md:text-2xl font-[500] leading-[18px]">
                AED {props?.data?.price}
              </div>
              <Button
                variant="rounded"
                className="font-[700] sm:font-[800] tracking-tight text-md text-sm md:text-base xl:text-lg "
              >
                {langContent[lang.lang].Index.productcard.ENTER_BTN}
              </Button>
            </div>
          </div>
        </div>
      </Link>
    )
  );
}

export default ProductCard;
