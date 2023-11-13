import React from 'react';
import Link from 'next/link';
import langContent from '~/locales';
import { useSelector } from 'react-redux';
import { RootState } from '~/store/store';

const Tabs = (props: any) => {
  const { lang } = useSelector((state: RootState) => state.layout);
  return (
    <div className="bg-card-foreground  mt-24">
      <div className="px-10 py-8 w-full hidden md:block">
        <div className="flex flex-row -m-4 text-center justify-center items-start lg:items-center gap-2 md:gap-14 lg:flex-row ">
          <div className="flex justify-center items-lg:flex-row center ">
            <Link href="#BuyTickets" className="font-sans">
              <button className="bg-background text-cardGray hover:bg-primary hover:text-black text-xs lg:text-base font-extrabold px-4 py-2 rounded-full tracking-tighter w-max">
                {langContent[lang.lang].ProductDetail.tabs.HEADING_ONE}
              </button>
            </Link>
          </div>
          {props?.comp_detail && props?.isCompetition ? (
            <div className="flex justify-center items-center ">
              <Link href="#CompititionDetail" className="font-sans">
                <button className="bg-background text-cardGray hover:bg-primary hover:text-black text-xs lg:text-base font-extrabold px-6 py-2 rounded-full tracking-tighter w-max">
                  <span className="hidden smm:inline-block">
                    {langContent[lang.lang].ProductDetail.tabs.HEADING_TWO}
                  </span>{' '}
                  {langContent[lang.lang].ProductDetail.tabs.SUB_HEADING_TWO}
                </button>
              </Link>
            </div>
          ) : null}

          {props?.data?.faq_id && props?.isFAQ ? (
            <div className="flex justify-center items-center ">
              <button className="bg-background text-cardGray hover:bg-primary hover:text-black text-xs lg:text-base font-extrabold px-6 py-2 rounded-full tracking-tighter">
                <Link href="#AccordianFaqs" className="font-sans">
                  {langContent[lang.lang]?.ProductDetail?.tabs?.HEADING_THREE}
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
