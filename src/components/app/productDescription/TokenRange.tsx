import React, { useEffect, useState } from 'react';
import { Slider } from '~/components/ui/slider';
import { Button } from '~/components/ui/button';
import { cn } from '~/utils/cn';
interface token {
  range: number[];
  setRange: any;
  min: number;
  max: number;
}
const TokenRange = ({ range, setRange, min, max }: token) => {
  console.log({ range });
  return (
    <div>
      <div className="range-slider-container mt-14">
        <Slider
          defaultValue={range}
          value={range}
          max={max}
          min={min}
          step={1}
          onValueChange={(e: any) => {
            console.log(e[0], '');
            setRange(e);
          }}
        />
        <br />
        <div
          className="absolute left-1/2 transform -translate-x-1/2 bottom-3 p-2 font-black bg-opacity-70  text-primary rounded-md text-lg whitespace-nowrap"
          style={{ left: `${range}%` }}
        >
          {range}
        </div>
      </div>
      <div className='flex justify-between'>
        <p className='text-card-gray font-black'>{min}</p>
        <p className='text-card-gray font-black'>{max}</p>
      </div>
    </div>
  );
};

export default TokenRange;
