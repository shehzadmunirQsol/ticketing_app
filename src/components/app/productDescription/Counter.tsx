import { Button } from '~/components/ui/button';
import TokenRange from './TokenRange';
import CounterStyle from './CounterStyle';
import { useDispatch, useSelector } from 'react-redux';
import { RootState } from '~/store/store';
import { useRouter } from 'next/router';
import { trpc } from '~/utils/trpc';
import { useToast } from '~/components/ui/use-toast';
import { addToCart } from '~/store/reducers/cart';
import { URIDecoder } from '~/utils/helper';
import langContent from '~/locales';
import { useEffect } from 'react';
import {
  URIGenerator,
  getAvailableTickets,
  renderNFTImage,
} from '~/utils/helper';

interface CounterProps {
  range: any;
  setRange: React.Dispatch<React.SetStateAction<number[]>>;
  user_ticket_limit: number;
  ticketInBasket: { current: number };
  ticketPurchased: number;
  perCustomerLimit: number;
  event: any;
}

export default function Counter(props: CounterProps) {

  var fullUrl = "";
  if(typeof window !== 'undefined'){
    // fullUrl = window.location.protocol + "//" + window.location.host;
    fullUrl = window.location.host;
  }

  const {
    range,
    setRange,
    user_ticket_limit,
    ticketInBasket,
    ticketPurchased,
    event,
    perCustomerLimit,
  } = props;
  const { user, isLogin } = useSelector((state: RootState) => state.auth);
  const { lang } = useSelector((state: RootState) => state.layout);
  const { cart } = useSelector((state: RootState) => state.cart);

  const { toast } = useToast();
  const { query } = useRouter();
  const dispatch = useDispatch();

  const addToBasket = trpc.cart.addToCart.useMutation();

  async function addToBasketHandler() {
    const { id } = URIDecoder(query?.id ?? '');
    const eventId = Number(id);

    const cartItem = cart?.cartItems?.find((item) => item.event_id === eventId);
    const payload = {
      subscription_type: cartItem?.subscription_type ?? null,
      cart_id: cart?.id ?? 0,
      event_id: eventId,
      is_subscribe: cartItem?.is_subscribe ?? false,
      quantity: parseInt(range) ?? 0,
    };



    const eventCartData = cart?.cartItems?.map((event) => ({
      id: event?.event_id,
      price: event?.Event?.price,
      name: event?.Event?.EventDescription[0]?.name,
      quantity: event?.quantity,
      image: renderNFTImage(event?.Event),
    }));

    console.log('dsdsdsd',eventCartData);


    try {
      if (isLogin) {
        const apiPayload = {
          ...payload,
          cart_item_id: cartItem?.id ?? 0,
          customer_id: user?.id,
        };
        const response = await addToBasket.mutateAsync(apiPayload);
        dispatch(addToCart(response.data));
      } else {
        const updatedCartItem = {
          ...payload,
          id: 0,
          Event: {
            thumb: event.thumb,
            price: event.price,
            end_date: event.end_date,
            tickets_sold: event.tickets_sold,
            user_ticket_limit: event.user_ticket_limit,
            total_tickets: event.total_tickets,
            category_id: event.category_id,
            is_enabled: event.is_enabled,

            EventDescription: [
              {
                name: event.EventDescription[0].name,
              },
            ],
          },
        };

        const updatedCart = {
          id: null,
          customer_id: null,
          cartItem: updatedCartItem,
        };
        dispatch(addToCart(updatedCart));
      }
      ticketInBasket.current = payload.quantity;

      toast({
        variant: 'success',
        title: 'Item added successfully!',
      });

      if ('sendinblue' in window && window?.sendinblue) {
        const eventData = {
          id: eventId,
          price: event.price,
          name: event.EventDescription[0].name,
          quantity: payload.quantity,
          image: process.env.NEXT_PUBLIC_MEDIA_BASE_URL + event?.thumb
        };
        eventCartData.push(eventData);
        

        const sendinblue: any = window.sendinblue;
        sendinblue?.track(
          'cart_updated',
          {
            "email": user.email,
            "FIRSTNAME": user.first_name
          },
          {
            "data": {
              "url" : fullUrl+"/cart",
              "item" : eventCartData,
            }
          },
        ) as any;



        console.log('pushed cart_updated to brevo 1',eventCartData);
      }
    } catch (error: any) {
      console.log({ error });
    }
  }

  const price = +(range[0] as number) * event?.price;
  // const price = +(range) * event?.price;


  useEffect(()=>{
    console.log(range,'yyy');
  })

  return (
    <div className="relative">
      {event?.tickets_sold >= event?.total_tickets ? (
        <div className="sm:p-4 space-y-4 grid items-center">
          <i className="fas fa-gauge-high text-7xl text-primary text-center" />
          <h3 className="text-base md:text-lg text-center text-white">
            SOLD OUT: All Tickets Have Been Purchased!
          </h3>
        </div>
      ) : ticketPurchased >= perCustomerLimit ? (
        <div className="sm:p-4 space-y-4 grid items-center">
          <i className="fas fa-gauge-high text-7xl text-primary text-center" />
          <h3 className="text-base md:text-lg text-center text-white">
            You have reached your max limit
          </h3>
        </div>
      ) : (
        <>
          <div className="flex items-center justify-between mt-3">
            <p className="text-base text-white">
              {langContent[lang.lang].ProductDetail.counter.TICKETS}
            </p>
            {ticketPurchased ? (
              <p className="text-sm text-white/40 ">
                Your Previous purchase: {' '}
                <strong className="text-primary">
                  {ticketPurchased?.toLocaleString()}
                </strong>{' '}
                tickets
              </p>
            ) : null}
          </div>
          <TokenRange
            range={range}
            setRange={setRange}
            min={1}
            max={user_ticket_limit}
          />
          <CounterStyle
            range={range}
            setRange={setRange}
            min={1}
            max={user_ticket_limit}
          />
          <p className="lg:text-3xl text-xl pl-0 text-primary font-bold pt-2">
          <span className="text-base">TOTAL AED</span> {(price ?? 0)?.toLocaleString()}
          </p>
          <div className="mt-3 mobfixedbtn">
            <Button
              className="w-full text-black font-sans font-bold h-10 md:h-12 text-base md:text-lg"
              variant="clip"
              onClick={addToBasketHandler}
              disabled={
                ticketInBasket.current === range[0] || addToBasket.isLoading
              }
            >
              {langContent[lang.lang].ProductDetail.counter.BASKET_BUTTON}
            </Button>
          </div>
        </>
      )}
    </div>
  );
}
