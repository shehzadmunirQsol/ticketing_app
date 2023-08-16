import React, { useRef } from 'react';
import ProductSection from './product_section';
import CategorySection from './product_category';
import HowtoSection from './how_to_play';
import WhyChoose from './why_choose';
import Testimonials from './testimonials';
import BannerSlider from './banner_slider';

function Home() {
  return (
    <div className="relative flex flex-col gap-8 min-h-screen w-screen  ">
      <BannerSlider />
      {/* product section 1 */}
      <div className="relative flex flex-col gap-8 min-h-screen w-screen px-6 py-2 ">
        <ProductSection
          class="max-w-sm lg:max-w-xs"
          slidesToShow={4}
          center={false}
          title={'Ending Soon Competitions'}
        />
        {/* product section 2 */}
        <ProductSection
          class="max-w-md lg:max-w-sm xl:max-w-md ml-2   "
          slidesToShow={3}
          center={true}
          title="UPCOMING COMPETITIONS"
        />
        <CategorySection />
      </div>
      <HowtoSection />
      <WhyChoose />
      <Testimonials />
    </div>
  );
}

export default Home;
