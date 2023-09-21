import ProductSection from './product_section';
import CategorySection from './product_category';
import HowtoSection from './how_to_play';
import WhyChoose from './why_choose';
import Testimonials from './testimonials';
import BannerSlider from './banner_slider';
import VideoSlider from './video_slider';

export default function Home() {
  return (
    <div className=" flex flex-col w-full ">
      {/* // <div className=""> */}
      <BannerSlider />

      {/* product section 1 */}
      <div className="relative flex flex-col gap-4 md:gap-14  px-4 md:px-10  py-6 md:py-12  ">
        {/* 13 cards */}
        <ProductSection
          class="mx-auto w-3/5 md:w-full"
          slidesToShow={3}
          center={false}
          title={'ENDING SOON COMPETITIONS'}
          type="closing"
          breakpoint={[3, 2, 1.5]}
          breakpointScreens={[1350, 1050, 800]}
        />
        {/* product section 2 */}
        {/* 11 cards */}
        {/* <div className="py-4"></div> */}
        <ProductSection
          class="mx-auto w-3/5 md:w-full"
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
      <div className="relative flex flex-col gap-6   px-4  py-12 lg:py-16 bg-background-footer ">
        <VideoSlider
          class="mx-auto w-full"
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
