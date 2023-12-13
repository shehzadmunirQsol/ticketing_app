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

export default function ProductListCard(props: CardInterface) {
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


  function format12HourTime(date: any) {
    if (!(date instanceof Date)) {
      return ''; // handle non-date values
    }
  
    const hours = date.getHours();
    const minutes = date.getMinutes();
    const ampm = hours >= 12 ? 'PM' : 'AM';
    const formattedHours = hours % 12 || 12; // Convert 0 to 12 for 12-hour format
  
    return `${formattedHours}:${minutes.toString().padStart(2, '0')} ${ampm}`;
  }

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
          className={`rounded-sm shadow-lg bg-card flex ${props?.class}`}
          ref={cardRef}
        >
          <div className="relative col">
            {isLastDay ? (
              <div className="font-bold absolute top-0 w-fit px-2 py-1 md:p-2 z-2 bg-primary text-black text-xxs md:text-sm">
                <span className="">CLOSES TODAY</span>
                {' '}
                {format12HourTime(props?.data?.end_date)}
                {' '}

                {/* {props?.data?.end_date?.getHours()?.toString()?.length > 1
                  ? props?.data?.end_date?.getHours()
                  : '0' + props?.data?.end_date?.getHours()}
                :
                {props?.data?.end_date?.getMinutes()?.toString()?.length > 1
                  ? props?.data?.end_date?.getMinutes()
                  : '0' + props?.data?.end_date?.getMinutes()}{' '} */}
              </div>
            ) : (
              ''
            )}
            <NextImage
              width={550}
              height={450}
              className="w-full h-[170px] sm:h-[230px] object-cover bg-white"
              src={props?.cash ?? renderNFTImage(props?.data) ?? CarImage}
              quality={100}
              alt="Sunset in the mountains"
            />

            {
              (Number(props?.data?.tickets_sold) / Number(props?.data?.total_tickets)) * 100 === 100 ?
                <span>SOLD</span>
                :
                null
            }

            <div className="bottlebx prodbottle">
              <NextImage src={BottleImage} alt="Sunset in the mountains" />
            </div>
          </div>

          <div className="col-60 px-3 py-2 md:py-3">
            <div className="flex flex-col mb-1">
              <div className="flex justify-between items-center mb-1">
                <span className="text-xs text-xxs text-gray-300">
                  {
                    props?.data?.tickets_sold && props?.data?.total_tickets && (
                      (() => {
                        const percentage =
                          (Number(props.data.tickets_sold) / Number(props.data.total_tickets)) * 100;
                        return percentage < 1 ? percentage.toFixed(2) : Math.round(percentage);
                      })()
                    )
                  }
                  % {langContent[lang.lang].Index.productcard.SOLD_TITLE}
                </span>
                <span className="text-xs text-xxs text-gray-300">
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
            <div className="h-5 md:h-auto overflow-hidden line-clamp-1 font-bold text-sm lg:text-xl mb-1">
              {/* {langContent[lang.lang].Index.productcard.WIN_TITLE ?? ''} */}
              <span className="text-gray-200  font-semibold">
                {props?.data?.EventDescription[0]?.name}
              </span>
            </div>
            <div className="relative w-full opacity-75  text-gray-200 text-xs md:text-sm font-light sm:font-normal leading-normal">
              <p className="h-14 overflow-hidden line-clamp-3">
                {customTruncate(props?.data?.EventDescription[0]?.desc, 100)} 
                {' '} or {' '}
                {langContent[lang.lang].Index.productcard.ALTERNATIVE_TITLE} 
                {' '} AED  {(props?.data?.cash_alt ?? 0)?.toLocaleString()}
              </p>
            </div>
            <hr className="opacity-20 mt-1 md:mt-4" />
            <div className="flex justify-between items-center mt-2 sm:mt-6 gap-4">
              <div className="text-primary text-base md:text-2xl font-[500] leading-[18px]">
                AED {props?.data?.price}
              </div>
              <Button
                variant="rounded"
                className="font-[700] sm:font-[800] tracking-tight text-xs md:text-base xl:text-lg h-7 md:h-10 px-3"
              >
                { 
                  (props && props.data && props.data.draw_date !== null) || ((Number(props?.data?.tickets_sold) / Number(props?.data?.total_tickets)) * 100 === 100) ?
                    langContent[lang.lang].Index.productcard.VIEW_BTN
                    :
                    langContent[lang.lang].Index.productcard.ENTER_BTN
                }
              </Button>
            </div>
          </div>
        </div>
      </Link>
    )
  );
}
