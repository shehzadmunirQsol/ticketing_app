import React, { useEffect, useState } from 'react';
import Image from 'next/image';
import CarImage from '../../../public/assets/card_image.png';
// import { Progress } from '@radix-ui/react-progress';
import { Progress } from '../../ui/progress';
import Counter from './Counter';
import ImageSliderStyle from './ImageSliderStyle';

const ImageSlider = () => {
  const [range, setRange] = useState<number[]>([1]);

  const price = range * 10 

  console.log(price,"price")

  return (
    <section className="text-gray-600 body-font">
      <div className="px-5  py-24 mx-auto flex flex-wrap">
        <div className="lg:w-1/2 w-full mb-10 lg:mb-0 rounded-lg overflow-hidden">
          <ImageSliderStyle />
        </div>
        <div className="flex flex-col flex-wrap lg:py-6 -mb-10 lg:w-1/2 md:w-full lg:pl-12 lg:text-left lg:pr-7">
          <div className="flex flex-col mb-10 lg:items-start items-start">
            <div className="p-2 z-2 inline-flex bg-primary text-black items-center justify-center   mb-5">
              <span className=" font-black mr-2">CLOSES TODAY </span> 20:00
            </div>
            <div className="flex-grow w-full">
              <div className="flex flex-col gap-2">
                <span className=" text-xs ">955 Sold out of 1850</span>
                <Progress value={80} className="w-full" />
              </div>
            </div>
            <div>
              <p className="mt-6 text-2xl  md:text-4xl xl:text-5xl font-normal tracking-[-2px] text-white  ">
                <span className=" font-black mr-1">WIN </span> this Blue Ferrari
                E63S Night Edition + AED1000 Cash!
              </p>
            </div>
            <div>
              <p className="lg:text-xl text-md text-white opacity-75 mt-6">
                Buy Evian Water Bottle and Donate to our Charity Partner
              </p>
            </div>
            <div className="flex flex-col lg:flex-row  py-6 mb-12 justify-between  w-full">
              <p className=" text-white text-xl  lg:text-2xl mb-2">
                Cash Alternative <span color=""></span>{' '}
                <span className=" font-black mr-1 text-primary">AED 1000</span>
              </p>
              <p className=" lg:text-2xl text-xl sm:pl-10 pl-0 text-primary font-black ">
                AED {price.tofi}
              </p>
            </div>
            <div className="w-full relative">
              <div className="z-50">
                <Counter range={range} setRange={setRange} />
              </div>
              {/* <div className="absolute bottom-10 -right-10  z-10  w-1/5 h-3/5  bg-teal-400 bg-opacity-50 rounded-full blur-3xl"></div> */}
            </div>
          </div>
        </div>
      </div>
    </section>
  );
};

export default ImageSlider;