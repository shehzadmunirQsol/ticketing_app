import React from 'react';

import Link from 'next/link';

const Tabs = (props: any) => {
  return (
    <div className="bg-card-foreground  mt-24">
      <div className="px-10 py-8 w-full hidden md:block">
        <div className="flex flex-row -m-4 text-center justify-center items-start lg:items-center gap-2 md:gap-14 lg:flex-row ">
          <div className="flex justify-center items-lg:flex-row center ">
            <Link href="#BuyTickets" className="font-sans">
              <button className="bg-background text-cardGray hover:bg-primary hover:text-black text-xs lg:text-base font-extrabold px-4 py-2 rounded-full tracking-tighter w-max">
                BUY TICKETS
              </button>
            </Link>
          </div>
          {props?.comp_detail ? (
            <div className="flex justify-center items-center ">
              <Link href="#CompititionDetail" className="font-sans">
                <button className="bg-background text-cardGray hover:bg-primary hover:text-black text-xs lg:text-base font-extrabold px-6 py-2 rounded-full tracking-tighter w-max">
                  <span className="hidden smm:inline-block">
                    {' '}
                    COMPETITIONS{' '}
                  </span>{' '}
                  DETAILS
                </button>
              </Link>
            </div>
          ) : null}

          {props?.data?.faq_id ? (
            <div className="flex justify-center items-center ">
              <button className="bg-background text-cardGray hover:bg-primary hover:text-black text-xs lg:text-base font-extrabold px-6 py-2 rounded-full tracking-tighter">
                <Link href="#AccordianFaqs" className="font-sans">
                  FAQs
                </Link>
              </button>
            </div>
          ) : null}
        </div>
      </div>
    </div>
  );
};

export default Tabs;
