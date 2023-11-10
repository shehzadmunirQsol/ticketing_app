import React from 'react';
import NextImage from '../ui/img';
const BannerTitle = ({ image, text }: any) => {
  return (
    <div className="w-full relative h-[250px] lg:h-[350px] z-40">
      <NextImage
        src={image}
        alt="/"
        fill
        quality={100}
        className="object-cover block  "
      />
      <div className="relative h-[35px] top-[50%] flex items-center">
        <p className="relative h-[35px] -top-[50%] text-white drop-shadow-2xl text-center w-full text-4xl  lg:text-5xl  uppercase font-[900]">
          {text}
        </p>
      </div>
    </div>
  );
};

export default BannerTitle;
