import Image, { StaticImageData } from 'next/image';
import React, { useEffect, useRef } from 'react';
// import Table from '~/components/common/table';
import CarImage from '~/public/assets/card_image.png';
import BottleImage from '~/public/assets/bottle.png';
// import { Progress } from '../ui/progress';
// import { Button } from '../ui/button';
import { renderNFTImage } from '~/utils/helper';
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
}

function WinnarsCard(props: cardInterface) {
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

  return (
    <div
      dir={props?.dir}
      className={`   rounded-sm overflow-hidden shadow-lg bg-card h-1/5 ${props?.class}`}
      ref={cardRef}
    >
      <div >
        <Image
          width={150}
          height={100}
          className="w-full h-full object-cover bg-white"
          src={props?.cash ?? renderNFTImage(props.data) ?? CarImage}
          quality={100}
          alt="Sunset in the mountains"
        />
       
      </div>

      <div className="px-6 mt-6 py-4">
        <div className=" text-3xl mb-2">
          <span className="text-gray-200   font-semibold leading-loose">
            {`${props?.data?.id} ${props?.data?.EventDescription[0]?.desc}`}
          </span>
          <span className="text-gray-200  leading-loose">
            {`${props?.data?.id} ${props?.data?.EventDescription[0]?.desc}`}
          </span>
        </div>
        <div className="opacity-75 text-gray-200  text-lg font-normal leading-normal">
          <p>
            winning tickets{' '}
            <span className="text-primary text-lg font-black leading-[18px] ml-6 ">
              #11611
            </span>
          </p>
          <p>
            draw date
            <span className="text-primary text-lg font-black leading-[18px] ml-16 ">
              06/08/2023
            </span>
          </p>
        </div>
        <hr className=" opacity-20 mt-4" />
        <div className=" mt-2">
          <p className='text-sm opacity-75 text-gray-200'>
            Carl Courtenage Won a Lamborghini Aventador and opted for the cash
            alternative!
          </p>
        </div>
      </div>
    </div>
  );
}

export default WinnarsCard;
