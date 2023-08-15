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
      <div className="flex overflow-x-auto">
        {/* Your grid items go here */}
        <div className="flex-none bg-blue-300 p-4 w-32">1</div>
        <div className="flex-none bg-green-300 p-4 w-32">2</div>
        <div className="flex-none bg-yellow-300 p-4 w-32">3</div>
        <div className="flex-none bg-red-300 p-4 w-32">4</div>
        <div className="flex-none bg-purple-300 p-4 w-32">5</div>
        <div className="flex-none bg-pink-300 p-4 w-32">6</div>
        <div className="flex-none bg-indigo-300 p-4 w-32">7</div>
        <div className="flex-none bg-orange-300 p-4 w-32">8</div>
        {/* Add more items as needed */}
      </div>
    </div>
  );
}

export default Home;
