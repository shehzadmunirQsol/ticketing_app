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
    <div className=" flex flex-col min-h-screen w-full ">
      {/* // <div className=""> */}
      <div className="relative top-0">
        <BannerSlider />
      </div>

      {/* product section 1 */}
      <div className="relative flex flex-col gap-4 md:gap-14  px-4 md:px-10  py-6 md:py-12  ">
        {/* 13 cards */}
        <ProductSection
          class=" mx-auto max-w-md md:max-w-[21.5rem] lg:max-w-[23rem] xl:max-w-[calc(100%-25px)] "
          slidesToShow={3.5}
          center={false}
          title={'ENDING SOON COMPETITIONS'}
          type="closing"
          breakpoint={[3, 2, 1]}
          breakpointScreens={[1350, 1050, 800]}
        />
        {/* product section 2 */}
        {/* 11 cards */}
        <div className="py-4"></div>
        <ProductSection
          class=" mx-auto max-w-md md:max-w-lg  xl:max-w-[calc(100%-25px)] max-h-md "
          slidesToShow={2.5}
          center={false}
          title="UPCOMING COMPETITIONS"
          type="upcomming"
          breakpoint={[2, 1, 1]}
          breakpointScreens={[900, 850, 600]}
        />
      </div>
      <CategorySection />
      <HowtoSection />
      <WhyChoose />
      <Testimonials />

      {/* Video section */}
      <div className="relative flex flex-col gap-6 md:gap-12  px-4 md:px-14 py-12 lg:py-16 bg-background-footer ">
        <VideoSlider
          class="mx-0 w-full max-w-[15rem] lg:max-w-[16rem] xl:max-w-[calc(100%-18px)]"
          slidesToShow={4}
          center={false}
          title="Winnar Wonders: "
          subTitle="A Glimpse of Excellence"
          breakpoint={[3, 2, 1]}
          breakpointScreens={[1100, 830, 500]}
        />
      </div>
    </div>
  );
}
