import React, { useEffect, useRef, useState } from 'react';
import { Progress } from '../../ui/progress';
import Counter from './Counter';
import ImageSliderStyle from './ImageSliderStyle';
import CountDown from './CountDown';
import { useSelector } from 'react-redux';
import { RootState } from '~/store/store';
import { useRouter } from 'next/router';
import langContent from '~/locales';

import {
  URIDecoder,
  customTruncate,
  getAvailableTickets,
} from '~/utils/helper';
import Glow from '~/components/common/glow';

const ImageSlider = ({ data, ticketPurchased }: any) => {

  const counterRef = React.useRef<any>(null);
  const { cart } = useSelector((state: RootState) => state.cart);
  const { lang } = useSelector((state: RootState) => state.layout);

  const [range, setRange] = useState<number[]>([10]);
  const { query } = useRouter();

  const ticketInBasket = useRef<number>(0);

  const { id: eventId } = URIDecoder(query?.id ?? '');

  const cartItem = cart?.cartItems?.find(
    (item) => item.event_id === +(eventId ?? 0),
  );

  useEffect(() => {
    if (cartItem) {
      ticketInBasket.current = cartItem.quantity ?? 0;
      setRange([cartItem.quantity ?? 0]);
    }
  }, [cartItem]);

  const price = +(range[0] as number) * data?.price;
  const percentageSold = (data?.tickets_sold / data?.total_tickets) * 100;

  const ticketEventPayload = {
    total_tickets: data?.total_tickets,
    tickets_sold: data?.tickets_sold ?? 0,
    user_ticket_limit: data?.user_ticket_limit,
  };

  const { userTicketLimit } = getAvailableTickets({
    event: ticketEventPayload,
    ticketPurchased: ticketPurchased,
    quantity: range[0] ?? 0,
  });

  useEffect(() => {
    counter();
  }, []);
  /**Counter */

  const counter = () => {
    // Set the date we're counting down to
    const countDownDate: any = 1698782400000; //new Date("Oct 31, 2023 24:00:00").getTime();

    // Update the count down every 1 second
    setInterval(() => {
      // Get todays date and time
      const now: any = new Date().getTime();

      // Find the distance between now an the count down date
      const distance: any = countDownDate - now;

      // Time calculations for days, hours, minutes and seconds
      const days: any = Math.floor(distance / (1000 * 60 * 60 * 24));
      const hours: any = Math.floor(
        (distance % (1000 * 60 * 60 * 24)) / (1000 * 60 * 60),
      );
      const minutes: any = Math.floor(
        (distance % (1000 * 60 * 60)) / (1000 * 60),
      );
      const seconds: any = Math.floor((distance % (1000 * 60)) / 1000);

      // Output the result in an element with id="demo"
      if (counterRef && counterRef.current!==null) {
        counterRef.current.innerHTML =
          '<div class="flex flex-row md:flex-col md:justify-start md:items-start space-x-2 md:space-y-4"><div class="flex space-x-1 md:space-x-4 flex-1 md:flex-none"><p class="timer-box flex flex-col gap-1 md:gap-3 items-center py-3 px-1 md:p-4 min-w-fit w-full border-2 border-primary rounded-xl"><span class="text-3xl sm:text-5xl font-normal text-primary">' +
          days +
          '</span><span class="text-xs sm:text-base text-white">' +
          'Days' +
          '</span></p> ' +
          '<p class="timer-box flex flex-col gap-1 md:gap-3 items-center py-3 px-1 md:p-4 min-w-fit w-full border-2 border-primary rounded-xl"><span class="text-3xl sm:text-5xl font-normal text-primary">' +
          hours +
          '</span><span class="text-xs sm:text-base text-white">' +
          'Hours' +
          '</span></p> ' +
          '<p class="timer-box flex flex-col gap-1 md:gap-3 items-center py-3 px-1 md:p-4 min-w-fit w-full border-2 border-primary rounded-xl"><span class="text-3xl sm:text-5xl font-normal text-primary">' +
          minutes +
          '</span><span class="text-xs sm:text-base text-white">' +
          'Mins' +
          '</span></p> ' +
          '<p class="timer-box flex flex-col gap-1 md:gap-3 items-center py-3 px-1 md:p-4 min-w-fit w-full border-2 border-primary rounded-xl"><span class="text-3xl sm:text-5xl font-normal text-primary">' +
          seconds +
          '</span><span class="text-xs sm:text-base text-white">' +
          'Secs' +
          '</span></p></div></div>';
      }
    }, 1000);
  };

  return (
    <section className="text-gray-600 body-font">
      <div className="py-4 mb-5 mx-auto flex flex-wrap">
        <div className="lg:w-1/2 w-full rounded-lg overflow-hidden">
          <ImageSliderStyle data={data} />
        </div>
        <div className="flex flex-col flex-wrap lg:py-6 -mb-10 lg:w-1/2 w-full lg:text-left bg-card px-5 py-6">
          <div className="flex flex-col lg:items-start items-start">
            <div className="flex-grow w-full">
              <div className="flex flex-col gap-2">
                <div className="flex justify-between items-center">

                <span className=" text-xs text-gray-300 ">
                  {Math.round(
                    (Number(data?.tickets_sold) / Number(data?.total_tickets)) *
                    100,
                    )}
                  % {langContent[lang.lang].ProductDetail.description.SOLD}
                </span>
                <span className='text-xs text-gray-300'>{(data?.tickets_sold)?.toLocaleString()} /{" "}{(data?.total_tickets)?.toLocaleString()}</span>
                    </div>
                <Progress value={percentageSold} className="w-full" />

              </div>
            </div>
            <div>
              <p className="mt-3 sm:mt-6 text-2xl  md:text-4xl xl:text-5xl font-normal tracking-[1px] sm:tracking-[-1px] text-white  ">
                <span className=" font-black mr-1">
                  {lang.lang_id === 2 ? 'يفوز' : 'WIN '}{' '}
                </span>
                {data?.EventDescription[0]?.name}
              </p>
            </div>
            <div className="flex flex-col lg:flex-row  mt-3 sm:mt-6 lg:items-center  justify-between  w-full">
              {data?.category_id == 1 && (
                <p className=" text-white text-xl  lg:text-2xl ">
                  {lang.lang_id === 2
                    ? 'البديل النقدي'
                    : 'Cash Prize Alternative '}{' '}
                  <span color=""></span>{' '}
                  <span className=" font-black mr-1 text-primary">
                    AED {(data?.cash_alt ?? 0)?.toLocaleString()}
                  </span>
                </p>
              )}

              <p className=" lg:text-2xl text-xl  pl-0 text-primary font-black ">
                AED {(price ?? 0)?.toLocaleString()}
              </p>
            </div>

            <div className="mb-4 py-3 sm:py-6">
              <p className="lg:text-xl text-md text-white opacity-75 ">
                {customTruncate(data?.EventDescription[0]?.desc, 100)}
              </p>
            </div> 

            <div className="w-full relative">
              <div className="relative z-10">
                <Counter
                  range={range}
                  ticketInBasket={ticketInBasket}
                  setRange={setRange}
                  user_ticket_limit={userTicketLimit}
                  ticketPurchased={ticketPurchased}
                  event={data}
                />
              </div>
              <Glow className="absolute bottom-0 -right-16   p-2   w-2/5 h-[180px]   " />
            </div>

            <CountDown dateString="1699646400000" />

          </div>
        </div>
      </div>
    </section>
  );
};

export default ImageSlider;
