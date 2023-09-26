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
      <div
        dir={props?.dir}
        className={`rounded-sm shadow-lg bg-card ${props?.class}`}
        ref={cardRef}
      >
        <div className="relative ">
          {endDate.toLocaleDateString() === todayDate.toLocaleDateString() ? (
            <div className=" absolute top-0 w-fit p-2 z-2 bg-primary text-black text-sm">
              <span className=" font-bold">CLOSES TODAY</span> 20:00
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
              {props?.data?.tickets_sold} Sold out of{' '}
              {props?.data?.total_tickets}
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
            {lang.lang_id === 1 ? (
              <span className="font-[800] text-gray-200 leading-loose">
                WIN
              </span>
            ) : lang.lang_id === 2 ? (
              <span className="font-[900] font-sans text-gray-200 leading-loose">
                يفوز
              </span>
            ) : (
              ''
            )}
            <span className="text-gray-200  font-semibold leading-loose mx-2 ">
              {props?.data?.EventDescription[0]?.name}
            </span>
          </div>
          <div className="relative w-full opacity-75  text-gray-200  text-md font-normal leading-normal ">
            <p className="  h-12  overflow-hidden ">
              {customTruncate(
                props?.data?.EventDescription[0]?.comp_details,
                100,
              )}
            </p>
          </div>
          <hr className=" opacity-20 mt-4" />

          {props?.data?.category_id === 1 ? (
            <div className=" mt-2">
              <span className="text-gray-200 text-md xl:text-lg font-normal leading-[18px]">
                Cash Alternative
              </span>
              <span className="text-primary text-md xl:text-lg font-black leading-[18px]">
                {' '}
                AED {(props?.data?.cash_alt ?? 0)?.toFixed(2)}
              </span>
            </div>
          ) : (
            spaceElement
          )}

          <div className="flex  justify-between items-center mt-8 gap-4">
            <div className="text-primary text-md xl:text-lg font-black leading-[18px]">
              AED {props?.data?.price?.toFixed(2)}
            </div>
            <Link href={`/product-detail/${props?.data?.id}`}>
              <Button
                variant="rounded"
                className="font-[800] tracking-tight text-md xl:text-lg "
              >
                ENTER NOW
              </Button>
            </Link>
          </div>
        </div>
      </div>
    )
  );
}

export default ProductCard;
