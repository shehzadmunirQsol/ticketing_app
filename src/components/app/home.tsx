import React from 'react';
import Slider from 'react-slick';

import 'slick-carousel/slick/slick.css';
import 'slick-carousel/slick/slick-theme.css';
import DataCard from '../common/card';
import { Card } from '../ui/card';
import ProductCard from '../common/card';

function Home() {
  const settings = {
    dots: true,
    infinite: true,
    speed: 500,
    slidesToShow: 1,
    slidesToScroll: 1,
  };
  return (
    <div>
      <div className=" w-full h-full text-white p-2">
        <ProductCard />
      
      </div>
    </div>
  );
}

export default Home;
