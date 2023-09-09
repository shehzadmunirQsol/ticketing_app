import React from 'react';
import Image from 'next/image';
import increment from '../../../public/assets/increment.svg';
import decrement from '../../../public/assets/decrement.svg';
interface token {
  range: number[];
  setRange: any;
  min: number;
  max: number;
}
const CounterStyle = ({ range, setRange, min, max }: token) => {
  const rangeType: any = range && range?.length ? range[0] : 1;
  const handlerCounter = (type: string) => {
    if (type == 'a' && rangeType < max) {
      setRange([rangeType + 1]);
    } else if (type == 'b' && rangeType > min) {
      setRange([rangeType - 1]);
    }
  };

  return (
    <div>
      <div className="flex items-center mt-10 text-center ">
        <button
          onClick={() => handlerCounter('b')}
          className="bg-primary lg:w-14 md:w-14 w-10 lg:h-14 md:h-14 h-10 rounded-l-sm "
        >
          <Image
            src={decrement}
            alt="decrement"
            className="w-10 mx-auto p-2 "
          />
        </button>

        <div className="flex justify-center border-2 border-backgroundEntires w-full items-center align-middle text-xl text-white  text-center bg-background lg:h-14 md:h-14 h-10 ">
          <p className="text-xs md:text-sm lg:text-xl">
            {' '}
            Number of tickets: <span className="font-black ">{range}</span>
          </p>
        </div>
        <button
          onClick={() => handlerCounter('a')}
          className="bg-primary lg:w-14 md:w-14 w-10 lg:h-14 md:h-14 h-10 rounded-l-sm "
        >
          <Image
            src={increment}
            alt="increment"
            className="lg:w-10 md:lg:w-10 w-6 mx-auto  p-1 md:lg:p-2 lg:p-2"
          />
        </button>
      </div>
    </div>
  );
};

export default CounterStyle;
