import Image, { StaticImageData } from 'next/image';
import React, { useEffect, useRef } from 'react';
// import Table from '~/components/common/table';
import CarImage from '~/public/assets/card_image.png';
import BottleImage from '~/public/assets/bottle.png';
import { Progress } from '../ui/progress';
import { Button } from '../ui/button';
import { renderNFTImage } from '~/utils/helper';
import { useSelector } from 'react-redux';
import { RootState } from '~/store/store';
interface cardInterface {
  class?: string;
  dir?: string;
  cash?: any;
  data?: any;
  nextPage?: () => void;
  isLast?: boolean;
}

function ProductCard(props: cardInterface) {
  console.log(props.data,"incards")
  const cardRef = useRef<HTMLDivElement>(null);
  const {lang}=useSelector((state:RootState)=>state.layout)

  /**
   * Implement Intersection Observer to check if the last Card in the array is visible on the screen, then set a new limit
   */
  useEffect(() => {
    if (!cardRef?.current) return;

    const observer = new IntersectionObserver(([entry]: any) => {
      if (props?.isLast && entry.isIntersecting) {
        if (props.nextPage) props?.nextPage();
        observer.unobserve(entry.target);
      }
    });

    observer.observe(cardRef.current);
  }, [props?.isLast]);

  return (
    <div
      dir={props?.dir}
      className={`   rounded-sm overflow-hidden shadow-lg bg-card h-1/5 ${props?.class}`}
      ref={cardRef}
    >
      <div className="relative">
        <div className=" absolute top-0 w-fit p-2 z-2 bg-primary text-black text-sm">
          <span className=" font-bold">CLOSES TODAY</span> 20:00
        </div>
        <Image
          width={150}
          height={100}
          className="w-full h-full object-cover bg-white"
          src={props?.cash ?? renderNFTImage(props.data) ?? CarImage}
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
          <span className=" text-xs ">955 Sold out of 1850</span>
          <Progress value={80} className="w-full" />
        </div>
        <div className="font-bold text-3xl mb-2">
          <span className="text-gray-200  font-semibold leading-loose">
            {props?.data?.EventDescription[0]?.name}
          </span>
        </div>
        <div className="opacity-75 text-gray-200  text-lg font-normal leading-normal">
          {props?.data?.EventDescription[0]?.desc}
        </div>
        <hr className=" opacity-20 mt-4" />
        <div className=" mt-2">
          <span className="text-gray-200 text-lg font-normal leading-[18px]">
            Cash Alternative
          </span>
          <span className="text-primary text-lg font-black leading-[18px]">
            {' '}
            AED 45000
          </span>
        </div>
        <div className="flex  justify-between items-center mt-6">
          <div className="text-primary text-lg font-black leading-[18px]">
            AED 120.00
          </div>
          <Button variant="rounded" className="font-[800] tracking-tight  ">
            ENTER NOW
          </Button>
        </div>
      </div>
      {/* <div className="px-6 pt-4 pb-2">
        <span className="inline-block bg-gray-200 rounded-full px-3 py-1 text-sm font-semibold text-gray-700 mr-2 mb-2">
          #photography
        </span>
        <span className="inline-block bg-gray-200 rounded-full px-3 py-1 text-sm font-semibold text-gray-700 mr-2 mb-2">
          #travel
        </span>
        <span className="inline-block bg-gray-200 rounded-full px-3 py-1 text-sm font-semibold text-gray-700 mr-2 mb-2">
          #winter
        </span>
      </div> */}
    </div>
  );
}

export default ProductCard;
