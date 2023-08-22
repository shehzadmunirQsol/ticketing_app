import Image from 'next/image';
import React from 'react';
const BannerTitle = ({ image, text }: any) => {
  return (
    <>
      <div className="w-full relative h-[300px] lg:h-[500px] ">
        <Image
          src={image}
          alt="/"
          fill
          quality={100}
          className=" object-cover block bg-testimonials"
        />
        <div className="relative   top-[60%] lg:top-[55%]">
          <p className="  text-white text-center w-full text-4xl  lg:text-5xl tracking-tighter  uppercase font-[900]">
            {text}
          </p>
        </div>
      </div>
    </>
  );
};

export default BannerTitle;
