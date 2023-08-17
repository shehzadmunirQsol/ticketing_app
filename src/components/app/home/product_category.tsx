import React, { useRef } from 'react';
import Slider from 'react-slick';

import 'slick-carousel/slick/slick.css';
import 'slick-carousel/slick/slick-theme.css';

import CarImage from '~/public/assets/card_image.png';

import Image from 'next/image';
interface producctInterface {
  class?: string;
  title: string;
  center: boolean;
  slidesToShow?: number;
}
function CategorySection() {
  return (
    <div className="  w-full bg-background  py-10 ">
      <div className=" grid sm:grid-cols-1 md:grid-cols-2  ">
        {[...Array(2)].map((i) => (
          <div
            key={i}
            className="mainContainer  group relative h-96  overflow-hidden cursor-pointer  "
          >
            <div className=" absolute  w-full  h-[100%] categoryClip  bg-primary opacity-40 transition-all ease-in-out "></div>
            <Image
              className="w-full h-full object-cover bg-white"
              src={CarImage}
              quality={100}
              alt="Sunset in the mountains"
            />
            <div className="absolute   top-0 p-4">
              <div className="w-1/5  text-gray-200 text-5xl font-black uppercase ">
                CARS GALORE
              </div>
            </div>
            <div className="absolute  w-full  bottom-4 p-4 flex gap-4 justify-between items-center">
              <div className=" text-gray-200 text-2xl font-extrabold leading-normal">
                Unveiling Our Automotive Giveaways
              </div>
              <div className=" text-gray-200 text-2xl font-extrabold leading-normal">
                <i className="fa-solid fa-arrow-right -rotate-45 group-hover:text-primary group-hover:rotate-0"></i>
              </div>
            </div>
          </div>
        ))}
      </div>
    </div>
  );
}

export default CategorySection;
