import React, { useState } from 'react';
import Slider from 'react-slick';

import 'slick-carousel/slick/slick.css';
import 'slick-carousel/slick/slick-theme.css';
import DataCard from '../common/card';
import { Card } from '../ui/card';
import ProductCard from '../common/card';
import BgImage1 from '../../public/assets/win-banner-1.png';
import BgImage2 from '../../public/assets/win-banner-2.jpeg';
import BgImage3 from '../../public/assets/win-banner-3.jpeg';
import Image from 'next/image';
import { Button } from '../ui/button';
function Home() {
  const [select, setSelect] = useState(2);

  const settings = {
    dots: true,
    infinite: true,
    speed: 500,
    slidesToShow: 1,
    slidesToScroll: 1,
  };

  const carSlider = [
    {
      image: BgImage3,
      text: '2021 BMW X5 40D & £1000',
      BannerTitle: 'WIN THIS BMW',
      BannerPrice: '1000 AED',
      BannerPara: 'Get your dream car at a fraction of the price!',
      BannerDate: 'WIN SUNDAY 8 PM',
    },
    {
      image: BgImage2,
      text: 'TOYOTA RAV4 2023',
      BannerTitle: 'WIN THIS TOYOTA',
      BannerPrice: '1000 AED',
      BannerPara: 'Get your dream car at a fraction of the price!',
      BannerDate: 'WIN SUNDAY 8 PM',
    },
    {
      image: BgImage1,
      text: 'NEW LAMBORGHINI',
      BannerTitle: 'WIN THIS LAMBORGHINI',
      BannerPrice: '1000 AED',
      BannerPara: 'Get your dream car at a fraction of the price!',
      BannerDate: 'WIN SUNDAY 8 PM',
    },
  ];

  console.log(`url('${carSlider[select]?.image.src}')`);

  return (
    <div className="relative h-[110vh] ">
      <div
        className="absolute -top-32 w-full h-[100vh] flex justify-between px-4 z-10"
        style={{
          background: `url('${carSlider[select]?.image.src}') no-repeat `,
          backgroundSize:"100vw 100vh",
        }}
      >
        {/* text content */}
        <div className="relative top-48 h-fit  ml-10 text-white max-w-[700px]">
          <p className="text-5xl font-[900] tracking-[-2px] ">
            {carSlider[select]?.BannerTitle}
          </p>
          {carSlider[select]?.BannerPrice ? (
            <p className="text-5xl tracking-[-2px] my-3 ">
              + {carSlider[select]?.BannerPrice} CASH
            </p>
          ) : (
            ''
          )}
          <p className="text-xl  font-normal ">
            {carSlider[select]?.BannerPara}
          </p>
          <p className="text-3xl tracking-[-2px] font-[900]  my-3">
            {carSlider[select]?.BannerDate}
          </p>
          <Button
            className="text-black font-sans font-[900]  tracking-[-1px]"
            variant="clip"
          >
            ENTER NOW
          </Button>
        </div>

        {/* text select cards */}
        <div className="relative w-fit -bottom-96 items-end h-fit flex justify-between  gap-3">
          {carSlider.map((item, i) => (
            <div
              className="group max-w-[120px] text-center font-semibold hover:cursor-pointer"
              onClick={() => setSelect(i)}
            >
              <div className="relative w-[100px] h-[60px] mx-auto border-2  border-transparent group-hover:bg-teal">
                <Image
                  src={item.image}
                  alt="/"
                  fill
                  className="rounded-md group-hover:rounded-none"
                />
              </div>
              <p className="mt-3">{item.text}</p>
            </div>
          ))}
        </div>
      </div>

      {/* <div className=" w-full h-full text-white p-2">
          <ProductCard />
        </div>

        <div className="  flex overflow-x-auto">
          <div className="flex-none bg-blue-300 p-4 w-32">1</div>
          <div className="flex-none bg-green-300 p-4 w-32">2</div>
          <div className="flex-none bg-yellow-300 p-4 w-32">3</div>
          <div className="flex-none bg-red-300 p-4 w-32">1</div>
          <div className="flex-none bg-purple-300 p-4 w-32">5</div>
          <div className="flex-none bg-pink-300 p-4 w-32">6</div>
          <div className="flex-none bg-indigo-300 p-4 w-32">7</div>
          <div className="flex-none bg-orange-300 p-4 w-32">8</div>
        </div> */}
    </div>
  );
}

export default Home;
