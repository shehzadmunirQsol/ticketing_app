import React from 'react';
import { Slider } from '~/components/ui/slider';

interface token {
  range: any;
  setRange: any;
  min: number;
  max: number;
}
const TokenRange = ({ range, setRange, min, max }: token) => {
  const percentage: any = ((range && range.length ? range[0] : 1) / max) * 100;

  // cons
  return (
    <div>
      <div className="range-slider-container mt-8 cursor-pointer">
        <Slider
          defaultValue={range}
          value={range}
          max={max}
          min={1}
          step={1}
          onValueChange={(e: any) => {
            setRange(e);
          }}
        />
        <br />
        <div
          className="absolute left-1/2  transform -translate-x-1/2 bottom-3 p-2 pl-6 font-bold bg-opacity-70  text-primary rounded-md text-lg whitespace-nowrap"
          style={{ left: `${percentage}%` }}
        >
          {range}
        </div>
      </div>
      <div className="flex justify-between" dir="ltr">
        <p className="text-card-gray font-bold text-white">
          {min?.toLocaleString()}
        </p>
        <p className="text-card-gray font-bold text-white">
          {max?.toLocaleString()}
        </p>
      </div>
    </div>
  );
};

export default TokenRange;
