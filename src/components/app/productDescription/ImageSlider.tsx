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

  const percentageSold = (data?.tickets_sold / data?.total_tickets) * 100;

  return (
    <section className="text-gray-600 body-font">
      <div className="py-4 mb-5 mx-auto flex flex-wrap">
        <div className="lg:w-1/2 w-full rounded-lg overflow-hidden md:pr-4">
          <ImageSliderStyle data={data} />
        </div>
        <div className="lg:py-6 -mb-10 lg:w-1/2 w-full lg:text-left bg-card px-5 py-6">
          <div className="flex flex-col items-start">
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
              <p className="mt-3 sm:mt-6 text-2xl  md:text-4xl xl:text-5xl font-normal tracking-[1px] sm:tracking-[-1px] text-white  ">
                <span className=" font-black mr-1">
                  {lang.lang_id === 2 ? 'يفوز' : 'WIN '}{' '}
                </span>
                {data?.EventDescription[0]?.name}
              </p>
            </div>
            <div className="flex flex-col lg:flex-row  mt-3 sm:mt-6 lg:items-center  justify-between  w-full">
              {data?.draw_date === null && data?.category_id == 1 && (
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
            </div>

            <div className="py-3 sm:py-4">
              <p className="lg:text-xl text-md text-white opacity-75 ">
                {customTruncate(data?.EventDescription[0]?.desc, 100)}
              </p>
            </div>
            {!data?.draw_date && data?.end_date?.getTime() > Date.now() ? (
              <>
                <div className="w-full relative z-10">
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
                <CountDown dateString={data?.end_date?.getTime()?.toString()} />
              </>
            ) : (
              <DisplayCounter data={data} />
            )}
          </div>
        </div>
      </div>
    </section>
  );
};

export default ImageSlider;

function DisplayCounter(props: { data: any }) {
  const { data } = props;

  let element: React.ReactNode;

  const winnerName = data?.Winner?.length
    ? data?.Winner[0]?.Customer?.first_name +
      ' ' +
      data?.Winner[0]?.Customer?.last_name
    : '';
  const ticketNumber = data?.Winner?.length ? data?.Winner[0]?.ticket_num : '';

  if (data?.draw_date) {
    element = (
      <div className="w-full space-y-2 grid items-center">
        <p className="text-base text-white/80">
          Drawn on the {data?.draw_date?.toDateString()}
        </p>
        <h3 className="text-base md:text-2xl text-white">
          Congratulations to {winnerName} with number {ticketNumber}
        </h3>
      </div>
    );
  } else {
    element = (
      <div className="w-full sm:p-4 space-y-4 grid items-center">
        <i className="fas fa-road-lock text-7xl lg:text-9xl text-primary text-center" />
        <h3 className="text-base md:text-xl lg:text-2xl text-center text-white">
          This Competition is Closed
        </h3>
      </div>
    );
  }
  return element;
}
