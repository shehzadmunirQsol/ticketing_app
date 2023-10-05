import React, { useRef } from 'react';
import Slider from 'react-slick';
import 'slick-carousel/slick/slick.css';
import 'slick-carousel/slick/slick-theme.css';
import Face1 from '~/public/assets/face/face1.png';
import Face2 from '~/public/assets/face/face2.png';
import Face3 from '~/public/assets/face/face3.png';
import Face4 from '~/public/assets/face/face4.png';
import Face5 from '~/public/assets/face/face5.png';
import Face6 from '~/public/assets/face/face6.png';
import Group15 from '~/public/assets/icons/Group15.png';
import LogoImage from '~/public/assets/logo.png';
import Image from 'next/image';
import { Button } from '~/components/ui/button';
import { RootState } from '~/store/store';
import { useSelector } from 'react-redux';
import langContent from '~/locales';


function Testimonials() {
  const { lang } = useSelector((state: RootState) => state.layout);

  const slider = useRef<Slider | null>(null);

  const next = () => {
    slider?.current?.slickNext();
  };
  const previous = () => {
    slider?.current?.slickPrev();
  };
  const settings = {
    className: 'center slider variable-width flex pb-8',

    dots: false,
    infinite: false,
    speed: 500,
    slidesToShow: 1,
    slidesToScroll: 1,
    arrows: false,
    centerMode: false,
    mobileFirst: true,
    responsive: [
      {
        breakpoint: 640,
        settings: {
          slidesToShow: 1.5,
          slidesToScroll: 1,
        },
      },
      {
        breakpoint: 480,
        settings: {
          slidesToShow: 1,
          slidesToScroll: 1,
        },
      },
    ],
  };

  return (
    <div className="relative flex flex-col sm:gap-14 justify-start w-full mx-auto mb-2 sm:py-4">
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
            {langContent[lang.lang].Index.testimonials.HEADING}

            </div>
            <div className="mt-2  text-center text-gray-200 text-5xl font-black leading-[48px]">
              {' '}
              <Image
                src={LogoImage}
                alt="Logo Image"
                width={150}
                height={140}
                className="mx-auto h-6  w-56 "
              />
            </div>
          </div>
        </div>
      </div>

      <div className="block sm:hidden py-6 space-y-8">
        <div
          className={`${
            lang?.dir == 'rtl' ? ' flex-row-reverse' : 'md:ml-0'
          }  flex gap-2 z-10 items-center justify-center `}
        >
          <Button
            variant="rounded"
            className="button prev-btn h-10 w-10 md:h-14 md:w-14"
            onClick={previous}
          >
            <i className="fa-solid fa-chevron-left"></i>
          </Button>
          <Button
            variant="rounded"
            className="button next-btn h-10 w-10 md:h-14 md:w-14"
            onClick={next}
          >
            <i className="fa-solid fa-chevron-right"></i>
          </Button>
        </div>
        <div className="z-30 px-4 relative h-full w-full mx-auto ">
          <Slider ref={slider} {...settings}>
            {langContent[lang.lang].Index.testimonials.array?.map((item, index) => {
              return (
                <Review
                  key={index}
                  {...item}
                  class={`${
                    langContent[lang.lang].Index.testimonials.array?.length != index + 1 ? 'xsm:mr-4' : ''
                  }  min-h-[14rem]`}
                  index={index}
                />
              );
            })}
          </Slider>
        </div>
      </div>

      <div className="hidden sm:grid grid-cols-2 lg:grid-cols-3 justify-start items-start gap-4 px-4 md:px-14 py-6 md:py-12 mb-4">
        {langContent[lang.lang].Index.testimonials.array.map((item, index) => {
          return <Review key={index} {...item} index={index} />;
        })}
      </div>
    </div>
  );
}

export default Testimonials;


type ReviewType = {
  class: string;
  name: string;
  desc: string;
  publish: string;
  stars: number;
  img: number;
  index: number;
};

function Review(item: ReviewType) {
  return (
    <div
      className={`relative flex  h-full  mdx:h-fit p-6 gap-x-4  sm:w-full border-t border-l  border-white/20 bg-testimonials backdrop-blur-lg rounded-md bg-clip-padding backdrop-filter  bg-opacity-10  ${item?.class}`}
    >
      <div className="z-10 h-12 w-14  rounded-full  bg-white">
        <Image
          className="w-full h-full object-cover rounded-full"
          // src={item.face}
          src={images[item.img] as any}
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
            <i key={index} className="fa-solid fa-star  text-yellow-400"></i>
          ))}
          <span className=" text-white opacity-70">{item.publish}</span>
        </div>
        <div className="text-xs">{item?.desc}</div>
      </div>
    </div>
  );
}


const images:any= {
  1:Face1,
  2:Face2,
  3:Face3,
  4:Face4,
  5:Face5,
  6:Face6
}
