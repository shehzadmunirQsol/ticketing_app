import React, { useEffect, useRef, useState } from 'react';
import { Progress } from '../../ui/progress';
import Counter from './Counter';
import ImageSliderStyle from './ImageSliderStyle';
import { useSelector } from 'react-redux';
import { RootState } from '~/store/store';
import { useRouter } from 'next/router';

const ImageSlider = ({ data, ticketPurchased }: any) => {
  const { cart } = useSelector((state: RootState) => state.cart);

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

  const availableTickets = data?.total_tickets - data?.tickets_sold;
  const userTicketLimit =
    availableTickets > data?.user_ticket_limit - ticketPurchased
      ? data?.user_ticket_limit - ticketPurchased
      : availableTickets;

  return (
    <section className="text-gray-600 body-font">
      <div className="px-5  py-24 mx-auto flex flex-wrap">
        <div className="lg:w-1/2 w-full mb-10 lg:mb-0 rounded-lg overflow-hidden">
          <ImageSliderStyle data={data} />
        </div>
        <div className="flex flex-col flex-wrap lg:py-6 -mb-10 lg:w-1/2 w-full lg:pl-12 lg:text-left lg:pr-7 pr-7 ">
          <div className="flex flex-col mb-10 lg:items-start items-start">
            <div className="p-2 z-2 inline-flex bg-primary text-black items-center justify-center   mb-5">
              <span className=" font-black mr-2">CLOSES TODAY </span> 20:00
            </div>
            <div className="flex-grow w-full">
              <div className="flex flex-col gap-2">
                <span className=" text-xs ">
                  {data?.tickets_sold} Sold out of {data?.total_tickets}
                </span>
                <Progress value={percentageSold} className="w-full" />
              </div>
            </div>
            <div>
              <p className="mt-6 text-2xl  md:text-4xl xl:text-5xl font-normal tracking-[-2px] text-white  ">
                <span className=" font-black mr-1">WIN </span>
                {data?.EventDescription[0]?.name}
              </p>
            </div>
            <div>
              <p className="lg:text-xl text-md text-white opacity-75 mt-6">
                Buy Evian Water Bottle and Donate to our Charity Partner
              </p>
            </div>
            <div className="flex flex-col lg:flex-row  py-6 mb-12 justify-between  w-full">
              <p className=" text-white text-xl  lg:text-2xl mb-2">
                Cash Alternative <span color=""></span>{' '}
                <span className=" font-black mr-1 text-primary">
                  AED {data?.cash_alt.toFixed(2)}
                </span>
              </p>
              <p className=" lg:text-2xl text-xl  pl-0 text-primary font-black ">
                AED {price.toFixed(2)}
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
