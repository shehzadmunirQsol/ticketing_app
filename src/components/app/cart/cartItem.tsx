import Image from 'next/image';
import BottleImage from '~/public/assets/bottle.png';
import { Switch } from '~/components/ui/switch';
import { Button } from '~/components/ui/button';
import { useState } from 'react';
import { trpc } from '~/utils/trpc';
import { useDispatch, useSelector } from 'react-redux';
import { useToast } from '~/components/ui/use-toast';
import { CartItemInterface, addToCart } from '~/store/reducers/cart';
import { getAvailableTickets, renderNFTImage } from '~/utils/helper';
import { RemoveItemDialog } from '~/components/common/modal/cartModal';
import {
  Tooltip,
  TooltipContent,
  TooltipProvider,
  TooltipTrigger,
} from '~/components/ui/tooltip';
import Link from 'next/link';
import { RootState } from '~/store/store';

type CartItemProp = {
  cartItem: CartItemInterface;
  cart_id: number;
  customer_id: number;
  ticketPurchased: number;
};

type SubscriptionType = 'weekly' | 'monthly' | 'quarterly' | null;

export default function CartItem(props: CartItemProp) {
  const { cart_id, customer_id, cartItem } = props;

  const { isLogin } = useSelector((state: RootState) => state.auth);
  const [isSubscribe, setIsSubscribe] = useState(cartItem?.is_subscribe);
  const [isModal, setIsModal] = useState(false);

  const [subscriptionType, setSubscriptionType] = useState<SubscriptionType>(
    cartItem?.subscription_type,
  );

  const { toast } = useToast();
  const dispatch = useDispatch();

  const addToBasket = trpc.cart.addToCart.useMutation();

  async function addToBasketHandler(
    type: 'increment' | 'decrement' | 'unsubscribe' | 'update_cart',
  ) {
    let quantity = cartItem.quantity;

    if (type === 'increment') quantity++;
    if (type === 'decrement') quantity--;

    const isSubscription = type === 'unsubscribe' ? false : isSubscribe;

    const payload = {
      subscription_type: isSubscription ? subscriptionType : null,
      cart_id: cart_id,
      event_id: cartItem?.event_id,
      is_subscribe: isSubscription,
      quantity,
    };

    try {
      if (isLogin) {
        const apiPayload = {
          ...payload,
          cart_item_id: cartItem?.id,
          customer_id: customer_id,
        };
        const response = await addToBasket.mutateAsync(apiPayload);
        dispatch(addToCart(response.data));
      } else {
        const updatedCartItem = {
          ...cartItem,
          ...payload,
        };
        const updatedCart = {
          id: cart_id,
          customer_id: customer_id,
          cartItem: updatedCartItem,
        };
        dispatch(addToCart(updatedCart));
      }
      setIsSubscribe(isSubscription);
      setSubscriptionType(isSubscription ? subscriptionType : null);

      toast({
        variant: 'success',
        title: 'Item updated successfully!',
      });
    } catch (error: any) {
      console.log({ error });
    }
  }

  function toggleSwitch() {
    if (isSubscribe && subscriptionType) addToBasketHandler('unsubscribe');
    else setIsSubscribe((prevValue) => !prevValue);
  }

  const ticketEventPayload = {
    total_tickets: props.cartItem?.Event?.total_tickets,
    tickets_sold: props.cartItem?.Event?.tickets_sold ?? 0,
    user_ticket_limit: props.cartItem?.Event?.user_ticket_limit,
  };

  const { isTicketLimit } = getAvailableTickets({
    event: ticketEventPayload,
    ticketPurchased: props?.ticketPurchased,
    quantity: cartItem?.quantity,
  });

  return (
    <div data-name="card" className="py-3 mdx:py-6 border-t border-white/40">
      <div className="mb-2 flex items-center justify-between mdx:hidden">
        <p className="text-xl font-bold">
          {cartItem?.Event?.EventDescription[0]?.name}
          {/* Win This 800BHP Ferrari E63s Night Edition + AED 1,000 Cash! */}
        </p>
        <i
          onClick={() => setIsModal((preModal) => !preModal)}
          className="fas fa-times cursor-pointer text-white/40 text-sm border min-w-[24px] min-h-[24px] inline-flex items-center justify-center rounded-full"
        />
      </div>

      <div className="flex items-start justify-between space-y-4">
        <div className="relative mr-10 min-w-[140px] min-h-[90px] mdx:min-w-[176px] mdx:min-h-[112px]">
          <Image
            src={renderNFTImage({ thumb: cartItem.Event.thumb })}
            fill
            alt={'car image'}
            className="w-full h-full absolute object-contain"
          />
          <div className="p-1 w-12 h-12 rounded-full overflow-hidden absolute top-[30%] -right-6 bg-gradient-to-b from-primary to-neutral-900">
            <Image
              src={BottleImage}
              alt={'car image'}
              className="w-12 h-12 object-cover  rounded-full bg-white"
            />
          </div>
        </div>
        <div className="flex-1 flex items-center justify-between space-x-4">
          <Link
            href={`/product-detail/${cartItem?.event_id}`}
            className="hidden flex-1 mdx:block text-xl xl:text-2xl "
          >
            {cartItem?.Event?.EventDescription[0]?.name}
            {/* Win This 800BHP Ferrari E63s Night Edition + AED 1,000 Cash! */}
          </Link>
          <div className="flex flex-col space-y-2">
            <div className="flex justify-between items-center min-w-[450px] w-1/2 max-w-[550px]">
              <div className="bg-card flex items-center justify-between overflow-hidden ">
                <Button
                  className="p-2 bg-primary text-background"
                  disabled={cartItem?.quantity === 1 || addToBasket.isLoading}
                  onClick={() => addToBasketHandler('decrement')}
                >
                  <i className="fas fa-minus text-xl xl:text-2xl font-extrabold" />
                </Button>
                <p className="w-16 text-center text-xl">{cartItem?.quantity}</p>

                <TooltipProvider>
                  <Tooltip open={isTicketLimit}>
                    <TooltipTrigger asChild>
                      <Button
                        className="p-2 bg-primary text-background"
                        disabled={isTicketLimit || addToBasket.isLoading}
                        onClick={() => addToBasketHandler('increment')}
                      >
                        <i className="fas fa-plus text-xl xl:text-2xl font-extrabold" />
                      </Button>
                    </TooltipTrigger>
                    <TooltipContent>
                      <p>Cannot buy more entries</p>
                    </TooltipContent>
                  </Tooltip>
                </TooltipProvider>
              </div>
              <p className="text-xl text-white font-bold">
                AED: {(cartItem?.quantity * cartItem?.Event?.price)?.toFixed(2)}{' '}
              </p>
              <div className="space-y-2">
                {true ? (
                  <></>
                ) : (
                  <div className="flex items-center gap-3">
                    <p className={isSubscribe ? 'text-white' : 'text-white/40'}>
                      Subscription
                    </p>
                    <Switch
                      disabled={addToBasket.isLoading}
                      onClick={toggleSwitch}
                      checked={isSubscribe}
                    />
                  </div>
                )}
              </div>
            </div>

            {isSubscribe ? (
              <div className="self-end grid grid-cols-2 gap-2">
                {Object.keys(subscription).map((frequency) => {
                  const subscriptionDate = new Date(
                    new Date().setDate(subscription[frequency] as number),
                  ).getTime();

                  const EndDate =
                    props?.cartItem?.Event?.end_date?.getTime() ?? 0;
                  const isSubscribable = subscriptionDate > EndDate;

                  return (
                    <Button
                      key={frequency}
                      className={`bg-card text-sm rounded-full ${
                        frequency?.toLocaleLowerCase() === subscriptionType
                          ? 'border border-primary'
                          : ''
                      }`}
                      disabled={isSubscribable}
                      variant="outline"
                      onClick={() =>
                        setSubscriptionType(
                          frequency?.toLocaleLowerCase() as SubscriptionType,
                        )
                      }
                    >
                      {frequency}
                    </Button>
                  );
                })}
                <Button
                  className="rounded-full text-sm font-extrabold "
                  disabled={addToBasket.isLoading || !subscriptionType}
                  onClick={() => addToBasketHandler('update_cart')}
                >
                  Update Cart
                </Button>
              </div>
            ) : null}
          </div>
          <i
            onClick={() => setIsModal((preModal) => !preModal)}
            className="hidden mdx:inline-flex fas fa-times cursor-pointer text-white/40 text-sm border min-w-[24px] min-h-[24px]  items-center justify-center rounded-full"
          />
        </div>
      </div>
      <RemoveItemDialog
        isModal={isModal}
        isLogin={isLogin}
        openChangeHandler={() => setIsModal((preModal) => !preModal)}
        cart_item_id={cartItem.id}
        event_id={cartItem.event_id}
        item_name={cartItem?.Event?.EventDescription[0]?.name ?? ''}
      />
    </div>
  );
}

const subscription: { [key: string]: number } = {
  Weekly: 7,
  Monthly: 30,
  Quarterly: 90,
};
