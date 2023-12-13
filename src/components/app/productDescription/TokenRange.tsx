import React, { useState, useEffect } from 'react';
// import { Slider } from '~/components/ui/slider';

interface token {
  range: any;
  setRange: any;
  min: number;
  max: number;
}
const TokenRange = ({ range, setRange, min, max }: token) => {
  const percentage: any = ((range && range.length ? range[0] : 1) / max) * 100;

  const [progress, setProgress] = useState(1);

  const handleSliderChange = (event: any) => {
    const myArray = [];
    const newValue = event.target.value;
    myArray.push(newValue);
    setRange(myArray);
    setProgress(newValue);
    updateSliderBackground(newValue);
  };

  const updateSliderBackground = (value: any) => {
    const percentage: any = (value / max) * 100;
    const sliderEl = document.getElementById('productrange');
    if (sliderEl) {
      sliderEl.style.background = `linear-gradient(to right, #20cba8 ${percentage}%, #17171a ${percentage}%)`;
    }
  };

  useEffect(() => {
    // Set the initial linear gradient background
    updateSliderBackground(progress);
  }, []);

  // cons
  return (
    <div>

      <div className="rangeslider mt-4 mb-1.5">
        <span className="slider-value greenText font-bold">{progress}</span>
        <input
          type="range"
          min="1"
          max={max}
          step={1}
          value={range}
          className="custom-range-slider"
          onChange={handleSliderChange}
          id="productrange"
        />
      </div>

      {/* <div className="range-slider-container mt-8 cursor-pointer">
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
      </div> */}
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
