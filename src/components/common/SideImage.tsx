import React from 'react';
import NextImage from '../ui/img';

const SideImage = ({ image, text, text2 }: any) => {
  return (
    <div className="relative max-w-[500px] ">
      <NextImage
        src={image.src}
        width={500}
        height={580}
        alt="image"
        className="h-auto object-cover rounded-sm"
      />
      <div className=" max-w-full h-full bg-white"></div>
      <div className="absolute ltr:left-0 rtl:right-0 top-0 px-5 pt-10">
        <p className="text-3xl w-72 ">{text}</p>
        <p className="font-black text-4xl uppercase mt-2">{text2}</p>
      </div>
    </div>
  );
};

export default SideImage;
