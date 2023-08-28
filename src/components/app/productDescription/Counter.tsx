import React, { useState } from 'react';
import { Slider } from '~/components/ui/slider';
import { Button } from '~/components/ui/button';
import TokenRange from './TokenRange';
import CounterStyle from './CounterStyle';

const Counter = ({range, setRange}) => {


  return (
    <div className="bg-backgroundDark p-4 z-auto ">
      <p className="text-lg text-white">How many tickets?</p>
      {/* <input type="range" name="range" id="range" className="appearance-white w-full h-3  rounded-full thumb:bg-primary" /> */}
      <div>
        <TokenRange range={range} setRange={setRange} min={1} max={100}/>
      </div>
      <div>
        <CounterStyle range={range} setRange={setRange} min={1} max={100}/>
      </div>
      <div className='mt-6'>
        <Button
          className="w-full  text-black font-sans font-[900]  text-xl tracking-[-1px]"
          variant="clip"
        >
          ADD TICKETS TO BASKET
        </Button>
      </div>
    </div>
  );
};

export default Counter;