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
  const { cart } = useSelector((state: RootState) => state.cart);
  const { lang } = useSelector((state: RootState) => state.layout);

  const [range, setRange] = useState<number[]>([10]);
  const { query } = useRouter();

  const ticketInBasket = useRef<number>(0);

  const { id: eventId } = URIDecoder(query?.id ?? '');

  const cartItem = cart?.cartItems?.find(
    (item) => item.event_id === +(eventId ?? 0),
  );

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
    if (cartItem) {
      ticketInBasket.current = cartItem.quantity ?? 0;
      setRange([cartItem.quantity ?? 0]);
    } else {
      setRange([userTicketLimit > 10 ? 10 : userTicketLimit]);
    }
  }, [cartItem, userTicketLimit]);

  const price = +(range[0] as number) * data?.price;
  const percentageSold = (data?.tickets_sold / data?.total_tickets) * 100;

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
                  perCustomerLimit={data?.user_ticket_limit}
                  ticketPurchased={ticketPurchased}
                  event={data}
                />
              </div>
              <Glow className="absolute bottom-0 -right-16   p-2   w-2/5 h-[180px]   " />
            </div>

            <CountDown dateString="1698782400000" />

          </div>
        </div>
      </div>
    </section>
  );
};

export default ImageSlider;
