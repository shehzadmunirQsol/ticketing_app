import React from 'react';

const Glow = ({ props }: any) => {
  return (
    <div className={`absolute bottom-0 ${props?.class} z-2  p-2  w-1/2 h-1/3  bg-teal-400 bg-opacity-30 rounded-full blur-3xl`}></div>
  );
};

export default Glow;
