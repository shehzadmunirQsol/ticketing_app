import React, { useRef } from 'react';
import ProductSection from './product_section';
import CategorySection from './product_category';
import HowtoSection from './how_to_play';
import WhyChoose from './why_choose';

function Home() {
  return (
    <div className="relative flex flex-col gap-8 min-h-screen w-screen  ">
      {/* product section 1 */}
      <div className="relative flex flex-col gap-8 min-h-screen w-screen px-6 py-2 ">
        <ProductSection
          class="max-w-sm lg:max-w-md"
          slidesToShow={3}
          center={false}
          title={'Ending Soon Competitions'}
        />
        {/* product section 2 */}
        <ProductSection
          class="max-w-xl lg:w-full   "
          slidesToShow={2}
          center={true}
          title="UPCOMING COMPETITIONS"
        />
        <CategorySection />
      </div>
      <HowtoSection />
      <WhyChoose />
    </div>
  );
}

export default Home;
