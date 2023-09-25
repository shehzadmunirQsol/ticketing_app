import React, { useEffect, useState } from 'react';
import { Slider } from '~/components/ui/slider';
import { Button } from '~/components/ui/button';
import { cn } from '~/utils/cn';
import { Maximize2 } from 'lucide-react';
interface token {
  range: any;
  setRange: any;
  min: number;
  max: number;
}
const TokenRange = ({ range, setRange, min, max }: token) => {
  const percentage: any = ((range && range.length ? range[0] : 1) / max) * 100;
  console.log({ percentage });
  console.log(range[0], 'range[0]');
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
      <div className="flex justify-between">
        <p className="text-card-gray font-black">{min?.toLocaleString()}</p>
        <p className="text-card-gray font-black">{max?.toLocaleString()}</p>
      </div>
    </div>
  );
};

export default TokenRange;
