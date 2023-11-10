import React from 'react';
import increment from '../../../public/assets/increment.svg';
import decrement from '../../../public/assets/decrement.svg';
import langContent from '~/locales';
import { useSelector } from 'react-redux';
import { RootState } from '~/store/store';
import NextImage from '~/components/ui/img';

interface token {
  range: number[];
  setRange: any;
  min: number;
  max: number;
}
const CounterStyle = ({ range, setRange, min, max }: token) => {
  const { lang } = useSelector((state: RootState) => state.layout);

  const rangeType: any = range && range?.length ? range[0] : 0;

  const handlerCounter = (type: string) => {
    if (type == 'a' && rangeType <= max - 1) {
      setRange([rangeType + 1]);
    } else if (type == 'b' && rangeType >= min + 1) {
      setRange([rangeType - 1]);
    }
  };

  return (
    <div>
      <div className="flex items-center my-6 sm:my-10 text-center" dir="ltr">
        <button
          onClick={() => handlerCounter('b')}
          className={`bg-primary lg:w-14 md:w-14 w-10 md:h-12 h-10  ${
            lang.lang === 'en' ? 'rounded-r-sm' : 'rounded-l-sm'
          }`}
        >
          <NextImage
            src={decrement}
            alt="decrement"
            className="w-10 mx-auto p-2 "
          />
        </button>

        <div className="flex justify-center border-2 border-backgroundEntires w-full items-center align-middle text-xl text-white  text-center bg-background md:h-12 h-10 ">
          <p className="inline-flex items-end gap-2 text-xs md:text-sm lg:text-xl">
            {' '}
            {
              langContent[lang.lang].ProductDetail.counter.NUMBERS_OF_TICKET
            }: <span className="font-black "> {range}</span>
          </p>
        </div>
        <button
          onClick={() => handlerCounter('a')}
          className={`bg-primary lg:w-14 md:w-14 w-10 md:h-12 h-10  ${
            lang.lang === 'en' ? 'rounded-l-sm' : 'rounded-r-sm'
          }`}
        >
          <NextImage
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
