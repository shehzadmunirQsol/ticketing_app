import 'slick-carousel/slick/slick-theme.css';
import 'slick-carousel/slick/slick.css';

import CarImage from '~/public/assets/card_image.png';

import { useSelector } from 'react-redux';
import Image from 'next/image';
import { trpc } from '~/utils/trpc';
import { RootState } from '~/store/store';
import { renderNFTImage } from '~/utils/helper';
import Link from 'next/link';
interface producctInterface {
  class?: string;
  title: string;
  center: boolean;
  slidesToShow?: number;
}
function CategorySection() {
  const { lang } = useSelector((state: RootState) => state.layout);

  const categoryFilters: any = {
    lang_id: lang.lang_id,
    rows: 2,
    first: 0,
  };

  const {
    data: categoryData,
    refetch: categoryDataRefetch,
    isFetched,
    isLoading,
    isError,
    isSuccess,
  } = trpc.category.get.useQuery(categoryFilters, {
    refetchOnWindowFocus: false,
  });

  console.log({ categoryData }, 'categoryData');
  // const categoryList = categoryData ? categoryData.data : [];

  return (
    <div className="  w-full bg-background  py-14   ">
      <div className=" grid sm:grid-cols-1 md:grid-cols-2  mx-auto w-fit">
        {categoryData?.data.map((item, i) => {
          const nameList = item.name.split(' ');
          return (
            <div
              key={i}
              className="mainContainer  group transition-all duration-300 relative h-64 md:h-96 cursor-pointer  "
            >
              <Link href={i == 0 ? `/cars` : i == 1 ? '/cash' : '/'}>
                <div className=" absolute  w-full  h-full categoryClip  bg-primary opacity-40 transition-all ease-in-out duration-300 "></div>
                <Image
                  className=" bg-white opacity-30  h-64 md:h-96 object-cover"
                  src={renderNFTImage(item)}
                  width={800}
                  height={800}
                  // fill
                  
                  quality={100}
                  alt="car"
                />

                <div className="absolute h-fit  top-0 p-4">
                  <div className="w-1/5  text-gray-200 text-5xl font-black uppercase ">
                    {nameList.map((name, index) => (
                      <p key={index}>{name}</p>
                    ))}
                    {/* <div className=''>
                  </div> */}
                  </div>
                </div>
                <div className="absolute  w-full  bottom-4 p-4 flex gap-4 justify-between items-center">
                  <div className=" text-gray-200 text-lg font-bold leading-normal text-ellipsis whitespace-nowrap overflow-hidden">
                    {item?.desc}
                  </div>
                  <div className=" text-primary text-2xl font-extrabold leading-normal">
                    <i className="fa-solid fa-arrow-right -rotate-45 group-hover:text-primary group-hover:rotate-0 duration-300"></i>
                  </div>
                </div>
              </Link>
            </div>
          );
        })}
      </div>
    </div>
  );
}

export default CategorySection;
