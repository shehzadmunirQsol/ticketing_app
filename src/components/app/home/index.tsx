import React, { useRef, useState } from 'react';
import ProductSection from './product_section';
import CategorySection from './product_category';
import HowtoSection from './how_to_play';
import WhyChoose from './why_choose';
import Testimonials from './testimonials';
import BannerSlider from './banner_slider';
import VideoSlider from './video_slider';
import { useSelector } from 'react-redux';
import { RootState } from '~/store/store';
import { trpc } from '~/utils/trpc';
import Glow from '~/components/common/glow';

export default function Home() {
  const { lang } = useSelector((state: RootState) => state.layout);

  const todayDate = new Date();
  console.log(todayDate, 'todayDate');

  const endingDate = new Date();
  endingDate.setDate(endingDate.getDate() + 7);
  console.log(endingDate, 'endingDate');
  const slide1=useRef<any>(null)
  const slide2=useRef<any>(null)

  // console.log(upcomingList?.data,"ip")

  return (
    <div className=" flex flex-col min-h-screen w-full max-w-[1600px] mx-auto">
      {/* // <div className=""> */}
      <div className="relative top-0">
        <BannerSlider />
      </div>

      {/* product section 1 */}
      <div className="relative flex flex-col gap-8  px-6 py-2 ">
        {/* 13 cards */}
        <ProductSection
          class="max-w-sm lg:max-w-xs"
          slidesToShow={4}
          center={false}
          title={'ENDING SOON COMPETITIONS'}
          type="closing"
          slide={slide1}
        />
        {/* product section 2 */}
        {/* 11 cards */}
        <ProductSection
          class="max-w-md lg:max-w-sm xl:max-w-md ml-2   "
          slidesToShow={3}
          center={false}
          title="UPCOMING COMPETITIONS"
          type="upcomming"
          slide={slide2}
        />
      </div>
      <CategorySection />
      <HowtoSection />
      <WhyChoose />
      <Testimonials />

      {/* Video section */}
      <div className="relative flex flex-col gap-8  px-6 py-12 bg-background-footer ">
        <VideoSlider
          class="max-w-md lg:max-w-sm xl:max-w-md"
          slidesToShow={4}
          center={false}
          title="Winnar Wonders: "
          subTitle="A Glimpse of Excellence"
        />
      </div>
    </div>
  );
}

// export default Home;
