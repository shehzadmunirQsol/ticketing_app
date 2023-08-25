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

export default function Home() {
  const { lang } = useSelector((state: RootState) => state.layout);

  const [filters, setFilters] = useState({
    lang_id: lang.lang_id,
    first: 0,
    rows: 9,
  });

  const todayDate = new Date();
  console.log(todayDate, 'todayDate');

  const endingDate = new Date();
  endingDate.setDate(endingDate.getDate() + 7);
  console.log(endingDate, 'endingDate');

  // console.log(upcomingList?.data,"ip")

  return (
    <div className=" flex flex-col gap-8 min-h-screen w-full max-w-[1600px] mx-auto">
      {/* // <div className=""> */}
      <div className="relative top-0">
        <BannerSlider />
      </div>

      {/* product section 1 */}
      <div className="relative flex flex-col gap-8  px-6 py-2 ">
        <ProductSection
          class="max-w-sm lg:max-w-xs"
          slidesToShow={4}
          center={true}
          title={'ENDING SOON COMPETITIONS'}
          type="closing"
        />
        {/* product section 2 */}
        <ProductSection
          class="max-w-md lg:max-w-sm xl:max-w-md ml-2   "
          slidesToShow={3}
          center={true}
          title="UPCOMING COMPETITIONS"
          type="upcomming"
        />
        <CategorySection />
      </div>
      <HowtoSection />
      <WhyChoose />
      <Testimonials />

      {/* Video section */}
      <div className="relative flex flex-col gap-8  px-6 py-12 bg-background-footer ">
        <VideoSlider
          class="max-w-md lg:max-w-sm xl:max-w-md    "
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
