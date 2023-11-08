import React, { useEffect, useRef, useState } from 'react';
import { Progress } from '../../ui/progress';
import Counter from './Counter';
import ImageSliderStyle from './ImageSliderStyle';
import ImageSliderStyleNew from './ImageSliderStyleNew';
import CountDown from './CountDown';
import { useSelector } from 'react-redux';
import { RootState } from '~/store/store';
import { useRouter } from 'next/router';
import langContent from '~/locales';
import Highlights from './highlights';

import {
  URIDecoder,
  customTruncate,
  getAvailableTickets,
} from '~/utils/helper';

const ImageSlider = ({ data, ticketPurchased, higlightmeta }: any) => {
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

  const percentageSold = (data?.tickets_sold / data?.total_tickets) * 100;

  const isMeta = higlightmeta;

  return (
    <section className="text-gray-600 body-font">
      <div className="detailbx">
        <div className="col-55">
          {/* <ImageSliderStyleNew data={data} /> */}
          <ImageSliderStyle data={data} />
        </div>
        <div className="col-40 bg-card">
          <div className="flex flex-col lg:items-start items-start">
            <div className="flex-grow w-full">
              <div className="flex flex-col gap-2">
                <div className="flex justify-between items-center">
                  <span className=" text-xs text-gray-300 ">
                    {Math.round(
                      (Number(data?.tickets_sold) /
                        Number(data?.total_tickets)) *
                        100,
                    )}
                    % {langContent[lang.lang].ProductDetail.description.SOLD}
                  </span>
                  <span className="text-xs text-gray-300">
                    {data?.tickets_sold?.toLocaleString()} /{' '}
                    {data?.total_tickets?.toLocaleString()}
                  </span>
                </div>
                <Progress value={percentageSold} className="w-full" />
              </div>
            </div>
            <div>
              <p className="mt-3 sm:mt-5 text-2xl md:text-3xl font-normal sm:tracking-[-1px] text-white">
                <span className=" font-bold mr-1">
                  {lang.lang_id === 2 ? 'يفوز' : 'WIN '}{' '}
                </span>
                {data?.EventDescription[0]?.name}
              </p>
            </div>
            <div className="flex flex-col lg:flex-row  mt-1 lg:items-center  justify-between  w-full">
              {data?.category_id == 1 && (
                <p className=" text-white text-xl">
                  {lang.lang_id === 2
                    ? 'البديل النقدي'
                    : 'Cash Prize Alternative '}{' '}
                  <span color=""></span>{' '}
                  <span className=" font-bold mr-1 text-primary">
                    AED {(data?.cash_alt ?? 0)?.toLocaleString()}
                  </span>
                </p>
              )}
            </div>

            <div className="pt-2 pb-1">
              <p className="text-base text-white opacity-75 ">
                {customTruncate(data?.EventDescription[0]?.desc, 100)}
              </p>
            </div>

            <div className="w-full relative">
              <div className="relative z-10">
                <Counter
                  range={range}
                  ticketInBasket={ticketInBasket}
                  setRange={setRange}
                  perCustomerLimit={data?.user_ticket_limit}
                  user_ticket_limit={userTicketLimit}
                  ticketPurchased={ticketPurchased}
                  event={data}
                />
              </div>
            </div>

            {data?.end_date?.getTime() > Date.now() ? (
              <CountDown dateString={data?.end_date?.getTime()?.toString()} />
            ) : null}

            {higlightmeta ? <Highlights meta={higlightmeta} /> : null }

          </div>
        </div>
      </div>
    </section>
  );
};

export default ImageSlider;
