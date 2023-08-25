import React from 'react';
import ImageSlider from './ImageSlider';
import EntiresDetail from './EntiresDetail';
import VideoSection from './VideoSection';
import LiveDraw from './LiveDraw';
import CompititionDetail from './CompititionDetail';
import Tabs from './Tabs';
import AccordianFaqs from './Faqs';
import LoginSignup from '../auth/LoginSignup';

const ProductDetail = () => {
  return (
    <div>
      <Tabs />
      <ImageSlider />
      <div className="lg:px-10 md:px-10 px-6">
        <EntiresDetail />
        <VideoSection />
      </div>
      <LiveDraw />
      <div className="lg:px-10 md:px-10 px-6 ">
        <CompititionDetail />
        <AccordianFaqs/>
      </div>
      <LoginSignup />
    </div>
  );
};

export default ProductDetail;
