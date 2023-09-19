import React, { useEffect, useRef, useState } from 'react';
import { Progress } from '../../ui/progress';
import Counter from './Counter';
import ImageSliderStyle from './ImageSliderStyle';
import { useSelector } from 'react-redux';
import { RootState } from '~/store/store';
import { useRouter } from 'next/router';
import { getAvailableTickets } from '~/utils/helper';

const ImageSlider = ({ data, ticketPurchased }: any) => {
  
  const { cart } = useSelector((state: RootState) => state.cart);
  const { lang } = useSelector((state: RootState) => state.layout)
  console.log(lang,"langlanglanglang")

  const [range, setRange] = useState<number[]>([1]);
  const { query } = useRouter();

  const ticketInBasket = useRef<number>(0);

  const cartItem = cart?.cartItems?.find(
    (item) => item.event_id === +(query?.id ?? 0),
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

  return (
    <section className="text-gray-600 body-font">
      <div className="py-4 mx-auto flex flex-wrap">
        <div className="lg:w-1/2 w-full mb-10 lg:mb-0 rounded-lg overflow-hidden">
          <ImageSliderStyle data={data} />
        </div>
        <div className="flex flex-col flex-wrap lg:py-6 -mb-10 lg:w-1/2 w-full lg:pl-12 lg:text-left lg:pr-7 pr-7 ">
          <div className="flex flex-col mb-10 lg:items-start items-start">
            <div className="p-2 z-2 inline-flex bg-primary text-black items-center justify-center   mb-5">
              <span className=" font-black mr-2"> {lang.lang_id === 2 ? " إغلاق اليوم " : "CLOSES TODAY " }        </span> 20:00
            </div>
            <div className="flex-grow w-full">
              <div className="flex flex-col gap-2">
                <span className=" text-xs text-white ">
                  {data?.tickets_sold} {lang.lang_id === 2 ? "بيعت من" : "Sold out of " } {data?.total_tickets}
                </span>
                <Progress value={percentageSold} className="w-full" />
              </div>
            </div>
            <div>
              <p className="mt-6 text-2xl  md:text-4xl xl:text-5xl font-normal tracking-[-2px] text-white  ">
                <span className=" font-black mr-1">{lang.lang_id === 2 ? "يفوز" : "WIN " } </span>
                {data?.EventDescription[0]?.name}
              </p>
            </div>
            <div>
              <p className="lg:text-xl text-md text-white opacity-75 mt-6">
              {lang.lang_id === 2 ? "قم بشراء زجاجة مياه إيفيان وتبرع لشريكنا الخيري" : "Buy Evian Water Bottle and Donate to our Charity Partner " }
              </p>
            </div>
            <div className="flex flex-col lg:flex-row  py-6 mb-12 justify-between  w-full">
              <p className=" text-white text-xl  lg:text-2xl mb-2">
              {lang.lang_id === 2 ? "البديل النقدي" : "Cash Alternative " } <span color=""></span>{' '}
                <span className=" font-black mr-1 text-primary">
                  AED {(data?.cash_alt ?? 0)?.toFixed(2)}
                </span>
              </p>
              <p className=" lg:text-2xl text-xl  pl-0 text-primary font-black ">
                AED {(price ?? 0)?.toFixed(2)}

              </p>
            </div>
            <div className="w-full relative">
              <div className="z-50">
                <Counter
                  range={range}
                  ticketInBasket={ticketInBasket}
                  setRange={setRange}
                  user_ticket_limit={userTicketLimit}
                  ticketPurchased={ticketPurchased}
                />
              </div>
            </div>
          </div>
        </div>
      </div>
    </section>
  );
};

export default ImageSlider;
