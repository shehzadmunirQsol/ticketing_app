import React, { useRef } from 'react';
import Slider from 'react-slick';

import 'slick-carousel/slick/slick.css';
import 'slick-carousel/slick/slick-theme.css';

import Group15 from '~/public/assets/icons/Group15.png';
import Group14 from '~/public/assets/icons/Group14.png';

import Face1 from '~/public/assets/face/face1.png';
import Face2 from '~/public/assets/face/face2.png';
import Face3 from '~/public/assets/face/face3.png';
import Face4 from '~/public/assets/face/face4.png';
import Face5 from '~/public/assets/face/face5.png';
import Face6 from '~/public/assets/face/face6.png';

import { User } from 'lucide-react';

import Image from 'next/image';
interface producctInterface {
  class?: string;
  title: string;
  center: boolean;
  slidesToShow?: number;
}
function Testimonials() {
  const TestimonialsData = [
    {
      step: 1,
      face: Face1,
      name: 'Adam S',
      stars: 5,
      publish: 'Last year',
      class: 'row-span-2',
      desc: '100% legit company for those sceptical people out there, I was one of them too to begin with but then i bagged 20k on my second time of playing. Money was in my account within 24 hours. Honestly these guys are genuinely nice people they were happy for me and made the process so easy and simple! I’ll be playing again put it that way…… got to be in it to win it!!!',
    },
    {
      step: 1,
      name: 'Oliver Bailey',
      stars: 5,
      class: 'row-span-1',
      face: Face2,
      publish: 'Last year',
      desc: '5* Won a Mercedes c63s AMG for just £10 off just 10 tickets on their raffle competitions!',
    },
    {
      step: 1,
      name: 'Andy Bone',
      class: 'row-span-1',
      face: Face3,
      stars: 5,
      publish: 'Last year',
      desc: 'Excellent friendly service and would highly recommend. Won a few times and goods arrived quickly. Cash alternative in bank within a few hours. Excellent and great.',
    },
    {
      step: 1,
      name: 'Tom Macca',
      stars: 5,
      class: 'row-span-2',
      face: Face4,
      publish: 'Last year',
      desc: 'Won the Toyota Supra which is a amazing car the staff where great will definitely be entering again',
    },
    {
      step: 1,
      name: 'Michael Jolley',
      stars: 5,
      class: 'row-span-2',
      face: Face5,

      publish: 'Last year',
      desc: "Wow what can I say I won an Audi RS5 absolutely over the moon, cracking blokes to deal with with brilliant communication, thanks once again for our car it's amazing all from a few £0:99p tickets. Is definitely recommend 7 days performance to anyone 10/10 well done guys",
    },
    {
      step: 1,
      name: 'Daniel Lloyd Mechanic',
      stars: 5,
      class: 'row-span-1',
      face: Face6,

      publish: 'Last year',
      desc: 'I have played in many competitions with 7 days the last few years and finally bagged a winner on tickets I forgot all about. I won the £5000 tui voucher but chose the cash instead.',
    },
  ];
  return (
    <div className="relative flex flex-col gap-14 justify-start   w-full  mx-auto mb-2 py-4">
      <div className="relative w-full m-auto flex flex-col items-center justify-center">
        <div className=" z-10 h-64 w-62 ">
          <Image
            className="w-full h-full object-contain "
            src={Group15}
            quality={100}
            alt="Sunset in the mountains"
          />
        </div>
        <div className="absolute bottom-0  ">
          <div className="relative bg-transparent">
            <div className="absolute top-0 p-8  w-full  bg-teal-400 bg-opacity-50 rounded-full blur-3xl"></div>
            <div className="text-center   text-white  text-xl ">
              See Why Our Customers Love
            </div>
            <div className="text-center text-gray-200 text-5xl font-black leading-[48px]">
              {' '}
              WINNAR
            </div>
          </div>
        </div>
      </div>

      <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 justify-start items-start gap-4 px-4 md:px-14 py-6 md:py-12 mb-4">
        {TestimonialsData?.map((item, index) => {
          return (
            <div
              className={`relative flex  h-full  mdx:h-fit p-6 gap-x-4  w-full border-t border-l  border-white/20 bg-testimonials backdrop-blur-lg rounded-md bg-clip-padding backdrop-filter  bg-opacity-10  ${item?.class}`}
              // className="relative flex   h-fit p-4 gap-4 items-start bg-primary   backdrop-filter backdrop-blur-md bg-opacity-10 rounded-md shadow-sm  "
              key={index}
            >
              <div className="   z-10 h-12 w-14  rounded-full  bg-white">
                <Image
                  className="w-full h-full object-cover rounded-full"
                  src={item.face}
                  quality={100}
                  alt="Sunset in the mountains"
                />
                {/* <User className='h-4 w-4 m-2'/> */}
              </div>
              <div className="flex flex-col gap-2 w-full">
                {/* name */}
                <div>{item?.name}</div>
                <div className="flex gap-2 text-xs items-center">
                  {Array.from(Array(+item?.stars), (_, index) => (
                    <i
                      key={index}
                      className="fa-solid fa-star  text-yellow-400"
                    ></i>
                  ))}
                  <span className=" text-white opacity-70">Last year</span>
                </div>
                <div className="text-xs">{item?.desc}</div>
              </div>
            </div>
          );
        })}
      </div>
    </div>
  );
}

export default Testimonials;
