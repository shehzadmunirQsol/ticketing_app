import React, { useRef, useState } from 'react';
import Slider from 'react-slick';

import 'slick-carousel/slick/slick.css';
import 'slick-carousel/slick/slick-theme.css';

import VideoCard from '../../common/video_card';
import { Button } from '../../ui/button';
import { useSelector } from 'react-redux';
import { RootState } from '~/store/store';
import { trpc } from '~/utils/trpc';

interface producctInterface {
  class?: string;
  title: string;
  subTitle: string;
  center: boolean;
  slidesToShow?: number;
}
function VideoSlider(props: producctInterface) {
  const { lang } = useSelector((state: RootState) => state.layout);
  const [videoCardSlider, setVideoCardSlider] = useState<Array<any>>([]);

  const initialOrderFilters: any = {
    lang_id: lang.lang_id,
    group: 'WONDER',
    is_enabled: true,
    rows: 10,
    first: 0,
    page: 0,
  };

  const {
    data: videoData,
    refetch: videoDataRefetch,
    isFetched,
    isLoading,
    isError,
    isSuccess,
  } = trpc.settings.get_banner.useQuery(initialOrderFilters, {
    refetchOnWindowFocus: false,
    onSuccess: () => {
      setVideoCardSlider(videoData || []);
    },

    // enabled: user?.id ? true : false,
  });
  console.log({ videoCardSlider });

  const slide = useRef<any>();
  const next = () => {
    slide?.current.slickNext();
  };
  const previous = () => {
    slide?.current?.slickPrev();
  };
  const settings = {
    className: 'center slider variable-width ',

    dots: false,
    infinite: props?.center,
    speed: 500,
    slidesToShow: props?.slidesToShow,
    slidesToScroll: props?.slidesToShow,
    centerMode: false,
    arrows: false,
    slidesPerRow: 1,
    initialSlide: 0,
    responsive: [
      {
        breakpoint: 1024,
        settings: {
          slidesToShow: 3,
          slidesToScroll: 2,
          initialSlide: 0,
        },
      },
      {
        breakpoint: 800,
        settings: {
          slidesToShow: 2,
          slidesToScroll: 1,
          initialSlide: 0,
          centerMode: false,
        },
      },

      {
        breakpoint: 640,
        settings: {
          initialSlide: 0,
          slidesToShow: 1,
          slidesToScroll: 1,
          centerMode: false,
        },
      },
    ],
  };
  return (
    <div className="relative  w-full ">
      <div className=" relative flex flex-col md:flex-row h-28 md:h-auto py-6  items-center w-full md:justify-between mb-6">
        <div>
          <p className="text-gray-200 !text-xl sm:!text-3xl lg:!text-5xl font-black uppercase  ">
            {props?.title}
          </p>
          <p className="text-gray-200 !text-xl sm:!text-3xl lg:!text-5xl font-light uppercase  ">
            {props?.subTitle}
          </p>
        </div>
        <div
          className={`${
            lang?.dir == 'rtl' ? ' flex-row-reverse' : 'md:absolute right-10'
          }  flex gap-2 items-center justify-center `}
        >
          <Button
            variant="rounded"
            className="button prev-btn h-10 w-10 md:h-14 md:w-14"
            onClick={() => previous()}
          >
            {/* <i className="fa-solid fa-left-arrow" /> */}
            <i className="fa-solid fa-chevron-left"></i>
          </Button>
          <Button
            variant="rounded"
            className="button next-btn h-10 w-10 md:h-14 md:w-14"
            onClick={() => next()}
          >
            <i className="fa-solid fa-chevron-right"></i>
          </Button>
        </div>
      </div>
      <div className="">
        <Slider ref={slide} {...settings}>
          {videoCardSlider
            ? videoCardSlider.map((item, index) => {
                return (
                  <div key={index} className="cardInfo">
                    <VideoCard
                      class={`${props?.class} `}
                      dir={`${lang?.dir}`}
                      data={item}
                    />
                  </div>
                );
              })
            : ''}
        </Slider>
      </div>
    </div>
  );
}

export default VideoSlider;
