import React from 'react';
import onlyImage from '../../../public/assets/only.svg';
import entiresImage from '../../../public/assets/entires.svg';
import wordwideImage from '../../../public/assets/wordwide.svg';
import maxpersonImage from '../../../public/assets/maxperson.svg';
import Image from 'next/image';
import Link from 'next/link';

const Tabs = () => {
  return (
    <div className="bg-card-foreground  mt-28">
      <div className="px-10 py-10 w-full">
        <div className="flex flex-row -m-4 text-center justify-center items-start lg:items-center  gap-6 lg:gap-14 md:gap-14   lg:flex-row ">
          <div className="flex justify-center items-lg:flex-row center ">
            <button className='bg-primary text-black text-xs lg:text-base  font-extrabold px-4 py-2 rounded-full '> BUY TICKETS</button>
          </div>
          <div className="flex justify-center items-center ">
            <button className='bg-background text-cardGray text-xs lg:text-base font-extrabold px-6 py-2 rounded-full'> 
            
            <Link href="#CompititionDetail">COMPETITIONS DETAILS</Link>
            </button>
          </div>
          <div className="flex justify-center items-center ">
            <button className='bg-background text-cardGray text-xs lg:text-base font-extrabold px-6 py-2 rounded-full'>
            <Link href="#AccordianFaqs">FAQs</Link>
               </button>
          </div>
          
        </div>
      </div>
    </div>
  );
};

export default Tabs;
