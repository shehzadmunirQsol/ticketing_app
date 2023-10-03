import React, { useEffect, useRef, useState } from 'react';
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
  breakpointScreens?: Array<number>;
  breakpoint?: Array<number>;
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

  const { data: videoData } = trpc.settings.get_banner.useQuery(
    initialOrderFilters,
    {
      refetchOnWindowFocus: false,

      // enabled: user?.id ? true : false,
    },
  );

  useEffect(() => {
    if (videoData?.data) {
      setVideoCardSlider(videoData?.data);
    }
  }, [videoData?.data]);

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
        breakpoint:
          props?.breakpointScreens && props?.breakpointScreens[0] !== undefined
            ? props?.breakpointScreens[0]
            : 1024,
        settings: {
          slidesToShow:
            props?.breakpoint && props?.breakpoint[0] !== undefined
              ? props?.breakpoint[0]
              : 3,
          slidesToScroll: 1,
          initialSlide: 0,
        },
      },
      {
        breakpoint:
          props?.breakpointScreens && props?.breakpointScreens[1] !== undefined
            ? props?.breakpointScreens[1]
            : 800,
        settings: {
          slidesToShow:
            props?.breakpoint && props?.breakpoint[1] !== undefined
              ? props?.breakpoint[1]
              : 2,
          slidesToScroll: 1,
          initialSlide: 0,
          centerMode: false,
        },
      },

      {
        breakpoint:
          props?.breakpointScreens && props?.breakpointScreens[2] !== undefined
            ? props?.breakpointScreens[2]
            : 600,
        settings: {
          slidesToShow:
            props?.breakpoint && props?.breakpoint[2] !== undefined
              ? props?.breakpoint[2]
              : 1,
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
    <div className=" relative  mx-auto  w-full ">
      <div className="mt-4 md:mt-6 relative flex flex-col md:flex-row h-28 md:h-auto py-6 justify-center w-full md:justify-between mb-6 gap-4 items-center ">
        <div>
          <p className="text-gray-200 text-center text-3xl ltr:sm:text-left rtl:sm:text-right  sm:text-5xl font-black uppercase  ">
            {props?.title}
          </p>
          <p className="text-gray-200 !text-xl sm:!text-3xl text-center ltr:sm:text-left rtl:sm:text-right lg:!text-5xl font-light uppercase  ">
            {props?.subTitle}
          </p>
        </div>
        <div
          className={`${
            lang?.dir == 'rtl' ? ' flex-row-reverse' : 'md:ml-0'
          }  flex gap-2 items-center justify-center `}
        >
          <Button
            variant="rounded"
            className={` ${'cursor-pointer'} button prev-btn h-10 w-10 md:h-14 md:w-14`}
            onClick={() => previous()}
            // disabled={videoCardSlider?.length === 4 ? true : false}
          >
            {/* <i className="fa-solid fa-left-arrow" /> */}
            <i className="fa-solid fa-chevron-left"></i>
          </Button>
          <Button
            variant="rounded"
            className={` ${'cursor-pointer'} button prev-btn h-10 w-10 md:h-14 md:w-14`}
            onClick={() => next()}
            // disabled={videoCardSlider?.length === 4 ? true : false}
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

          {videoCardSlider.length === 0 ? (
            <div className="text-center w-full py-10 text-lg">
              Coming Soon...
            </div>
          ) : (
            ''
          )}
        </Slider>
      </div>
    </div>
  );
}

export default VideoSlider;
