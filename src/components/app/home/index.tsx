import ProductSection from './product_section';
import CategorySection from './product_category';
import HowtoSection from './how_to_play';
import WhyChoose from './why_choose';
import Testimonials from './testimonials';
import BannerSlider from './banner_slider';
import InstagramFeed from './instagram_feed';
import VideoSlider from './video_slider';
import langContent from '~/locales';
import { RootState } from '~/store/store';
import { useSelector } from 'react-redux';

export default function Home() {
  const { lang } = useSelector((state: RootState) => state.layout);
  return (
    <div className=" flex flex-col w-full ">
      {/* // <div className=""> */}
      <BannerSlider />

      {/* product section 1 */}
      <div className="relative flex flex-col gap-6 md:gap-14 px-4 md:px-14 py-4 md:py-14">
        {/* 13 cards */}
        <ProductSection
          class="mx-auto w-3/5 md:w-full"
          slidesToShow={3}
          center={false}
          title={langContent[lang.lang].Index.products.HEADING}
          type="closing"
          breakpoint={[3, 2, 1.5]}
          breakpointScreens={[1350, 1050, 800]}
        />
        {/* product section 2 */}
        {/* 11 cards */}
        <ProductSection
          class="mx-auto w-3/5 md:w-full"
          slidesToShow={3}
          center={false}
          title={langContent[lang.lang].Index.upcoming.HEADING}
          type="upcoming"
          breakpoint={[3, 2, 1.5]}
          breakpointScreens={[1350, 1050, 800]}
        />
      </div>
      
      <CategorySection />
      <HowtoSection />
      <WhyChoose />
      <Testimonials />

      <InstagramFeed 
      title={langContent[lang.lang].Index.wonders.HEADING}
      subTitle={langContent[lang.lang].Index.wonders.SUB_HEADING}
      />

      {/* Video section */}
      {/* <div className="relative bg-background-footer px-4 flex flex-col gap-4 md:gap-14  md:px-14  py-6 md:py-12">
        <VideoSlider
          class="mx-auto w-full"
          slidesToShow={4}
          center={false}
          title={langContent[lang.lang].Index.wonders.HEADING}
          subTitle={langContent[lang.lang].Index.wonders.SUB_HEADING}
          breakpoint={[3, 2, 1]}
          breakpointScreens={[1100, 830, 500]}
        />
      </div> */}
      
    </div>
  );
}
