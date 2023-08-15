import Image from 'next/image';
import React from 'react';
import Table from '~/components/common/table';
import CarImage from '~/public/assets/card_image.png';
import BottleImage from '~/public/assets/bottle.png';
import { Progress } from '../ui/progress';
import { Button } from '../ui/button';

function ProductCard() {
  return (
    <div className="group max-w-sm rounded-sm overflow-hidden shadow-lg bg-card">
      <div className="relative">
        <div className=" absolute top-0 w-1/2 p-2 z-2 bg-teal text-black text-sm">
          <span className=" font-bold">CLOSES TODAY</span> 20:00
        </div>
        <Image
          className="w-full h-full object-cover bg-white"
          src={CarImage}
          quality={100}
          alt="Sunset in the mountains"
        />
        <div className="absolute -bottom-8 left-4 rounded-full w-1/5 p-1 bg-gradient-to-b from-teal to-neutral-900">
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
          <span className="text-gray-200  font-black leading-loose">WIN </span>
          <span className="text-gray-200 font-normal leading-loose">
            Hyundai Aura
          </span>
        </div>
        <div className="opacity-75 text-gray-200  text-lg font-normal leading-normal">
          Buy Evian Water Bottle and make it yours!
        </div>
        <hr className=" opacity-20 mt-4" />
        <div className=" mt-2">
          <span className="text-gray-200 text-lg font-normal leading-[18px]">
            Cash Alternative
          </span>
          <span className="text-teal text-lg font-black leading-[18px]">
            {' '}
            AED 45000
          </span>
        </div>
        <div className="flex  justify-between items-center mt-6">
          <div className="text-teal text-lg font-black leading-[18px]">
            AED 120.00
          </div>
          <Button variant="outline" size="icon_square">
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
