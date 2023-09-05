import React, { useRef } from 'react';
import ProductSection from './product_section';
import CategorySection from './product_category';
import HowtoSection from './how_to_play';
import WhyChoose from './why_choose';
import Testimonials from './testimonials';
import BannerSlider from './banner_slider';
import VideoSlider from './video_slider';
import Glow from '~/components/common/glow';

export default function Home() {
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
        />
        {/* product section 2 */}
        {/* 11 cards */}
        <ProductSection
          class="max-w-md lg:max-w-sm xl:max-w-md ml-2   "
          slidesToShow={3}
          center={false}
          title="UPCOMING COMPETITIONS"
          type="upcomming"
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
