import React from 'react';
import { useForm } from 'react-hook-form';
import Image from 'next/image';

const SideImage = ({ image, text, text2 }: any) => {
  const form = useForm<any>();
  return (
    <div className="relative h-4/5 max-w-[500px] max-h-[700px]">
      <Image
        src={image.src}
        width={51500}
        height={51500}
        alt="image"
        className="w-full h-full   object-contain"
      />
      <div className=" max-w-full h-full bg-white"></div>
      <div className="absolute left-0 top-0 px-5 pt-10">
        <p className="text-3xl w-72 ">{text}</p>
        <p className="font-black text-4xl uppercase mt-2">{text2}</p>
      </div>
    </div>
  );
};

export default SideImage;
