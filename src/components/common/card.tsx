import Image from 'next/image';
import React, { useEffect, useRef } from 'react';
import CarImage from '~/public/assets/card_image.png';
import BottleImage from '~/public/assets/bottle.png';
import { Progress } from '../ui/progress';
import { Button } from '../ui/button';
import { customTruncate, renderNFTImage } from '~/utils/helper';
import { useSelector } from 'react-redux';
import { RootState } from '~/store/store';
import Link from 'next/link';
import langContent from '~/locales';

interface cardInterface {
  class?: string;
  dir?: string;
  cash?: any;
  data?: any;
  nextPage?: () => void;
  isLast?: boolean;
  isCash?: boolean;
}

function ProductCard(props: cardInterface) {
  const cardRef = useRef<HTMLDivElement>(null);
  const { lang } = useSelector((state: RootState) => state.layout);
  const todayDate = new Date();
  const endDate = new Date(props?.data?.end_date);

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

  const spaceElement = props?.isCash ? null : <div className="h-8" />;

  return (
    props?.data && (
      <Link href={`/product-detail/${props?.data?.id}`}>
        <div
          dir={props?.dir}
          className={`rounded-sm shadow-lg bg-card ${props?.class}`}
          ref={cardRef}
        >
          <div className="relative ">
            {endDate.toISOString().split('T')[0] ==
            todayDate.toISOString().split('T')[0] ? (
              <div className=" absolute top-0 w-fit p-2 z-2 bg-primary text-black text-sm">
                <span className=" font-bold">CLOSES TODAY</span>{' '}
                {endDate?.getHours()}:{endDate?.getMinutes()}
              </div>
            ) : (
              ''
            )}
            <Image
              width={550}
              height={450}
              className="w-full h-[230px] object-cover bg-white"
              src={props?.cash ?? renderNFTImage(props?.data) ?? CarImage}
              quality={100}
              alt="Sunset in the mountains"
            />
            <div className="absolute -bottom-8 left-4 rounded-full w-20 p-1 bg-gradient-to-b from-primary to-neutral-900">
              <Image
                className="w-full h-full object-cover  rounded-full bg-white"
                src={BottleImage}
                alt="Sunset in the mountains"
              />
            </div>
          </div>

          <div className="px-6 mt-6 py-4">
            <div className="flex flex-col gap-1">
              <span className=" text-xs ">
                {Math.round(
                  (Number(props?.data?.tickets_sold) /
                    Number(props?.data?.total_tickets)) *
                    100,
                )}
                % {langContent[lang.lang].Index.productcard.SOLD_TITLE}
              </span>
              <Progress
                value={
                  (Number(props?.data?.tickets_sold) /
                    Number(props?.data?.total_tickets)) *
                  100
                }
                className="w-full"
              />
            </div>
            <div className="font-bold text-xl lg:text-2xl xl:text-3xl line-clamp-1">
            {langContent[lang.lang].Index.productcard.WIN_TITLE ?? ""}
              
              <span className="text-gray-200  font-semibold leading-loose mx-2 ">
                {props?.data?.EventDescription[0]?.name}
              </span>
            </div>
            <div className="relative w-full opacity-75  text-gray-200  text-md font-normal leading-normal ">
              <p className="  h-12  overflow-hidden ">
                {customTruncate(props?.data?.EventDescription[0]?.desc, 100)}
              </p>
            </div>
            <hr className=" opacity-20 mt-4" />

            {props?.data?.category_id === 1 && props?.data?.cash_alt ? (
              <div className=" mt-2">
                <span className="text-gray-200 text-md xl:text-lg font-semibold leading-[18px]">
                  {langContent[lang.lang].Index.productcard.ALTERNATIVE_TITLE}
                </span>
                <span className="text-primary text-md xl:text-lg font-black leading-[18px]">
                  {' '}
                  AED {(props?.data?.cash_alt ?? 0)?.toLocaleString()}
                </span>
              </div>
            ) : (
              spaceElement
            )}

            <div className="flex  justify-between items-center mt-8 gap-4">
              <div className="text-primary text-md xl:text-lg font-black leading-[18px]">
                AED {props?.data?.price}
              </div>
              <Button
                variant="rounded"
                className="font-[800] tracking-tight text-md xl:text-lg "
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
