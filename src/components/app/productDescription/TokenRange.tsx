import React, { useEffect, useState } from 'react';
import { Slider } from '~/components/ui/slider';
import { Button } from '~/components/ui/button';
import { cn } from '~/utils/cn';
import { Maximize2 } from 'lucide-react';
interface token {
  range: number[];
  setRange: any;
  min: number;
  max: number;
  data:object
}
const TokenRange = ({ range, setRange, min, max,data }: token) => {
  console.log({ range });
  const totalValue = 500; // Total value
const currentValue = 100; // Current dynamic value

const percentage = (range[0] / max) * 100;
console.log({percentage})
// cons
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
          style={{ left: `${percentage}%` }}
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
