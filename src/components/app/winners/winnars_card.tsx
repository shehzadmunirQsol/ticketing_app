import React, { useEffect, useRef } from 'react';
import WinnerImage from '~/public/assets/winner.svg';
import langContent from '~/locales';
import { useSelector } from 'react-redux';
import { RootState } from '~/store/store';
import NextImage from '~/components/ui/img';

interface cardInterface {
  class?: string;
  dir?: string;
  cash?: any;
  data?: any;
  nextPage?: () => void;
  isLast?: boolean;
}

function WinnarsCard(props: cardInterface) {
  const { lang } = useSelector((state: RootState) => state.layout);

  const cardRef = useRef<HTMLDivElement>(null);

  /**
   * Implement Intersection Observer to check if the last Card in the array is visible on the screen, then set a new limit
   */
  useEffect(() => {
    if (!cardRef?.current) return;

    const observer = new IntersectionObserver(([entry]: any) => {
      if (props?.isLast && entry.isIntersecting) {
        if (props?.nextPage) props?.nextPage();
        observer.unobserve(entry.target);
      }
    });

    observer.observe(cardRef.current);
  }, [props?.isLast]);

  // date handle
  const actualDate = new Date(props?.data?.draw_date);
  const options: any = { year: 'numeric', month: '2-digit', day: '2-digit' };
  const formattedDate = actualDate.toLocaleDateString('en-US', options);

  return (
    <div
      dir={props?.dir}
      className={`rounded-sm shadow-lg bg-card ${props?.class}`}
      ref={cardRef}
    >
      <div className="relative w-full h-56">
        <NextImage
          fill
          className=" w-full h-full object-cover bg-white"
          src={
            props?.data?.thumb
              ? `${process.env.NEXT_PUBLIC_MEDIA_BASE_URL}${props?.data?.thumb}`
              : WinnerImage
          }
          alt={props?.data?.Event?.EventDescription[0]?.name ?? ''}
          quality={100}
        />
      </div>

      <div className="px-4 mt-6 py-4">
        <div className=" text-lg md:text-2xl lg:text-3xl mb-2">
          <p className="text-gray-200 overflow-hidden h-14 md:h-24 lg:h-28 ">
            {langContent[lang.lang].Winners.CONGRATULATIONS}{' '}
            <span className="font-bold">
              {props?.data?.Customer?.first_name}
            </span>{' '}
            {langContent[lang.lang].Winners.CONGRATS_WINNING}{' '}
            <span className="font-bold">
              {props?.data?.Event?.EventDescription[0]?.name}
            </span>
          </p>
        </div>
        <div className=" text-lightColor lg:text-lg md:text-lg text-sm font-normal leading-normal">
          <p>
            {langContent[lang.lang].Winners.WINNER_TICKET}{' '}
            <span className="text-primary lg:text-lg md:text-lg text-sm font-black leading-[18px] ml-6 ">
              #{props?.data?.ticket_num}
            </span>
          </p>
          <p>
            {langContent[lang.lang].Winners.WINNER_DATE}
            <span className="text-primary lg:text-lg md:text-lg text-sm font-black leading-[18px] ml-16 ">
              {formattedDate}
            </span>
          </p>
        </div>
        <hr className=" opacity-20 mt-4" />
        <div className=" mt-2">
          <p className="text-sm opacity-75 text-grayColor">
            {props?.data?.Customer?.first_name}{' '}
            {langContent[lang.lang].Winners.WINNER_SUB_HEADING}{' '}
            {props?.data?.Event?.EventDescription[0]?.name}
          </p>
        </div>
      </div>
    </div>
  );
}

export default WinnarsCard;
