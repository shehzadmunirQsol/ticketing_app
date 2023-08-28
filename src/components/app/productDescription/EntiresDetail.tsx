import React from 'react';
import onlyImage from '../../../public/assets/only.svg';
import entiresImage from '../../../public/assets/entires.svg';
import wordwideImage from '../../../public/assets/wordwide.svg';
import maxpersonImage from '../../../public/assets/maxperson.svg';
import Image from 'next/image';

const EntiresDetail = () => {
  return (
    <div className="bg-backgroundDark border-2 rounded-md border-backgroundEntires">
      <div className="container px-10 py-10 w-full">
        <div className="flex flex-col -m-4 text-center justify-center items-start lg:items-center lg:justify-between gap-14 lg:gap-0   lg:flex-row">
          <div className="flex justify-center items-center ">
            <Image src={onlyImage}  className="mr-4"  alt={"image"} />
            <p className="leading-relaxed lg:text-xl md:text-xl text-lg text-primary font-semibold">
              Entries only <span className="font-black">AED 20</span>
            </p>
          </div>
          <div className="flex justify-center items-center ">
            <Image src={entiresImage} className="mr-4" alt={"image"} />
            <p className="leading-relaxed lg:text-xl md:text-xl text-lg text-primary font-semibold">
              Max Entries <span className="font-black">99999</span>
            </p>
          </div>
          <div className="flex justify-center items-center ">
            <Image src={maxpersonImage} className="mr-4" alt={"image"} />
            <p className="leading-relaxed lg:text-xl md:text-xl text-lg text-primary font-semibold">
              Max <span className="font-black">500</span> per person only
            </p>
          </div>
          <div>
            <div className="flex justify-center items-center  ">
              <Image src={wordwideImage} className="mr-4" alt={"image"} />
              <div className="text-start">
                <p className="leading-relaxed  lg:text-xl md:text-xl text-lg text-primary font-semibold">
                  <span className="font-black">Worldwide </span>Shipping
                </p>
                <p className="text-white text-sm underline -mt-2 ">
                  {`T & C's Apply`}
                </p>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
};

export default EntiresDetail;
