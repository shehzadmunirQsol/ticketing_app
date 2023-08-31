import { Button } from '~/components/ui/button';
import TokenRange from './TokenRange';
import CounterStyle from './CounterStyle';
import Glow from '~/components/common/glow';
import { useDispatch, useSelector } from 'react-redux';
import { RootState } from '~/store/store';
import { useRouter } from 'next/router';
import { trpc } from '~/utils/trpc';
import { useToast } from '~/components/ui/use-toast';
import { addToCart } from '~/store/reducers/cart';

interface CounterProps {
  range: number[];
  setRange: React.Dispatch<React.SetStateAction<number[]>>;
  user_ticket_limit: number;
  ticketInBasket: { current: number };
}
const Counter: React.FC<CounterProps> = ({
  range,
  setRange,
  user_ticket_limit,
  ticketInBasket,
}) => {
  const { user, isLogin } = useSelector((state: RootState) => state.auth);
  const { cart } = useSelector((state: RootState) => state.cart);

  const { toast } = useToast();
  const { query } = useRouter();
  const dispatch = useDispatch();

  const addToBasket = trpc.cart.addToCart.useMutation();

  async function addToBasketHandler() {
    if (!isLogin) {
      toast({
        variant: 'destructive',
        title: 'Login to your account first!',
      });
      return;
    }

    const cartItem = cart?.cartItems?.find(
      (item) => item.event_id === +(query?.id ?? 0),
    );
    const payload = {
      subscription_type: cartItem?.subscription_type ?? null,
      cart_item_id: cartItem?.id ?? 0,
      customer_id: user?.id,
      cart_id: cart?.id ?? 0,
      event_id: +(query?.id ?? 0),
      is_subscribe: cartItem?.is_subscribe ?? false,
      quantity: range[0] ?? 0,
    };

    try {
      const response = await addToBasket.mutateAsync(payload);
      console.log({ response });
      dispatch(addToCart(response.data));

      ticketInBasket.current = payload.quantity;

      toast({
        variant: 'success',
        title: 'Item added successfully!',
      });

      console.log({ response });
    } catch (error: any) {
      console.log({ error });
    }
  }

  return (
    <div className="relative bg-backgroundDark p-4  ">

      <p className="text-lg text-white">How many tickets?</p>
      {/* <input type="range" name="range" id="range" className="appearance-white w-full h-3  rounded-full thumb:bg-primary" /> */}
      <div>
        <TokenRange
          range={range}
          setRange={setRange}
          min={1}
          max={user_ticket_limit}
        />
      </div>
      <div>
        <CounterStyle
          range={range}
          setRange={setRange}
          min={1}
          max={user_ticket_limit}
        />
      </div>
      <div className="mt-6">
        <Button
          className="w-full  text-black font-sans font-[900]  text-xl tracking-[-1px]"
          variant="clip"
          onClick={addToBasketHandler}
          disabled={ticketInBasket.current === range[0]}
        >
          ADD TICKETS TO BASKET
        </Button>
      </div>

    </div>
  );
};

export default Counter;
