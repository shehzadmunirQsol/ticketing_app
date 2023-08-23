import 'slick-carousel/slick/slick-theme.css';
import 'slick-carousel/slick/slick.css';

import CarImage from '~/public/assets/card_image.png';

import { useSelector } from 'react-redux';
import Image from 'next/image';
import { trpc } from '~/utils/trpc';
import { RootState } from '~/store/store';
interface producctInterface {
  class?: string;
  title: string;
  center: boolean;
  slidesToShow?: number;
}
function CategorySection() {
  const { lang } = useSelector((state: RootState) => state.layout);


  return (
    <div className="  w-full bg-background  py-10 ">
      <div className=" grid sm:grid-cols-1 md:grid-cols-2  ">
        {[...Array(2)].map((i) => (
          <div
            key={i}
            className="mainContainer  group transition-all duration-300 relative h-96  overflow-hidden cursor-pointer  "
          >
            <div className=" absolute  w-full  h-[100%] categoryClip  bg-primary opacity-40 transition-all ease-in-out duration-300 "></div>
            <Image
              className="w-full h-full object-cover bg-white"
              src={CarImage}
              quality={100}
              alt="car"
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
                <i className="fa-solid fa-arrow-right -rotate-45 group-hover:text-primary group-hover:rotate-0 duration-300"></i>
              </div>
            </div>
          </div>
        ))}
      </div>
    </div>
  );
}

export default CategorySection;
