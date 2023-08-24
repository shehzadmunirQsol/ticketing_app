import React from 'react';
import { Slider } from '~/components/ui/slider';
import { Button } from '~/components/ui/button';
import Image from 'next/image';
import increment from '../../../public/assets/increment.svg';
import decrement from '../../../public/assets/decrement.svg';

const CounterStyle = ({ range, setRange, min, max }) => {
  const handlerCounter = (type: String) => {
    if (type == 'a' && range[0] <= max) {
      setRange([range[0] + 1]);
    } else if (type == 'b' && range[0] > min) {
      setRange([range[0] - 1]);
    }
  };

  return (
    <div>
      <div className="flex items-center mt-10 text-center ">
        <div onClick={() => handlerCounter('a')}>
          <button className="bg-primary lg:w-14 md:w-14 w-10 lg:h-14 md:h-14 h-10 rounded-l-sm ">
            <Image
              src={increment}
              alt="increment"
              className="lg:w-10 md:lg:w-10 w-6 mx-auto  p-1 md:lg:p-2 lg:p-2"
            />
          </button>
        </div>
        <div className="flex justify-center border-2 border-backgroundEntires w-full items-center align-middle text-xl text-white  text-center bg-background lg:h-14 md:h-14 h-10 ">
         <p className='text-xs md:text-sm lg:text-xl'> Number of tickets: <span className="font-black ">{range}</span></p>
        </div>
        <div onClick={() => handlerCounter('b')}>
          <button className="bg-primary lg:w-14 md:w-14 w-10 lg:h-14 md:h-14 h-10 rounded-l-sm ">
            <Image
              src={decrement}
              alt="decrement"
              className="w-10 mx-auto p-2 " 
            />
          </button>
        </div>
      </div>
    </div>
  );
};

export default CounterStyle;
