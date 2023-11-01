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

interface CounterProps {
  range: number[];
  setRange: React.Dispatch<React.SetStateAction<number[]>>;
  user_ticket_limit: number;
  ticketInBasket: { current: number };
  ticketPurchased: number;
  perCustomerLimit: number;
  event: any;
}

const Counter: React.FC<CounterProps> = ({
  range,
  setRange,
  user_ticket_limit,
  ticketInBasket,
  ticketPurchased,
  event,
  perCustomerLimit,
}) => {
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
      quantity: range[0] ?? 0,
    };

    const eventCartData = cart?.cartItems?.map((event) => ({
      id: event?.event_id,
      price: event?.Event?.price,
      name: event?.Event?.EventDescription[0]?.name,
      quantity: event?.quantity,
    }));

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
        };
        eventCartData.push(eventData);
        const sendinblue: any = window.sendinblue;

        sendinblue?.track(
          'cart_updated' /*mandatory*/,
          JSON.stringify({}) /*user data optional*/,
          JSON.stringify({
            cart_id: cart.id,
            data: eventCartData,
          }) /*optional*/,
        ) as any;

        console.log('pushed cart_updated to brevo');
      }
    } catch (error: any) {
      console.log({ error });
    }
  }

   const price = +(range[0] as number) * event?.price;

  return (
    <div className="relative">
      {ticketPurchased >= perCustomerLimit ? (
        <div className="sm:p-4 space-y-4 grid items-center">
          <i className="fas fa-gauge-high text-7xl lg:text-9xl text-primary text-center" />
          <h3 className="text-base md:text-xl lg:text-2xl text-center text-white">
            You have reached your max limit
          </h3>
        </div>
      ) : (
        <>
          <div className="flex items-center justify-between">
            <p className="text-lg text-white">
              {langContent[lang.lang].ProductDetail.counter.TICKETS}
            </p>
            {ticketPurchased ? (
              <p className="text-sm text-white/40 ">
                {"You've"} purchased{' '}
                <strong className="text-primary">
                  {ticketPurchased?.toLocaleString()}
                </strong>{' '}
                tickets
              </p>
            ) : null}
          </div>
          <p className=" lg:text-2xl text-xl  pl-0 text-primary font-black pt-1">
            AED {(price ?? 0)?.toLocaleString()}
          </p>
          <TokenRange
            range={range}
            setRange={setRange}
            min={0}
            max={user_ticket_limit}
          />
          <CounterStyle
            range={range}
            setRange={setRange}
            min={1}
            max={user_ticket_limit}
          />
          <div className="mt-6">
            <Button
              className="w-full text-black font-sans font-[900]  tracking-[-1px] h-12 text-sm xs:text-xl"
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
};

export default Counter;
