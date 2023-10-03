import Image from 'next/image';
import React from 'react';
import { useSelector } from 'react-redux';
import { RootState } from '~/store/store';
const BannerTitle = ({ image, text }: any) => {
  const { lang } = useSelector((state: RootState) => state.layout);

  return (
    <>
      <div className="w-full relative h-[250px] lg:h-[350px] z-40">
        <Image
          src={image}
          alt="/"
          fill
          quality={100}
          className="object-cover block bg-black/50" 
        />
        <div className="relative h-[35px] top-[50%] flex items-center">
          <p className="relative h-[35px] -top-[50%] text-white drop-shadow-2xl text-center w-full text-4xl  lg:text-5xl  uppercase font-[900]">
            {text}
          </p>
        </div>
      </div>
    </>
  );
};

export default BannerTitle;
