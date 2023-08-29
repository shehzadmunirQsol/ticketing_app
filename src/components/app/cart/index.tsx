import Image from 'next/image';
import BottleImage from '~/public/assets/bottle.png';
import { Button } from '~/components/ui/button';
import { Switch } from '~/components/ui/switch';
import { useState } from 'react';
import { Input } from '~/components/ui/input';
import ProductSection from '../home/product_section';
import Glow from '~/components/common/glow';
import { trpc } from '~/utils/trpc';
import { RootState } from '~/store/store';
import { useDispatch, useSelector } from 'react-redux';
import { useToast } from '~/components/ui/use-toast';
import { CartItemInterface, addToCart } from '~/store/reducers/cart';
import { renderNFTImage } from '~/utils/helper';
import { RemoveItemDialog } from '~/components/common/modal/cartModal';

export default function CartPage() {
  const { cart } = useSelector((state: RootState) => state.cart);

  return (
    <div className="relative">
      <div className="pt-24"></div>
      <div className="relative bg-background py-16 px-14 space-y-14">
        <h2 className="text-5xl font-bold uppercase">Basket</h2>
        <div data-name="cards" className="w-full border-b border-white/40">
          {cart?.cartItems?.map((cartItem) => (
            <CartItem
              key={cartItem.id}
              cartItem={cartItem}
              cart_id={cart?.id ?? 0}
              customer_id={cart?.customer_id ?? 0}
            />
          ))}
        </div>
        <div className="bg-background space-y-4 w-1/3 ml-auto">
          <div className="flex bg-card border border-border">
            <Input
              placeholder="Coupon code"
              className="px-4 flex-1 bg-transparent border-none z-10 "
            />
            <Button
              variant={'ghost'}
              className="text-primary border-l border-border z-10 "
            >
              Apply Coupon
            </Button>
          </div>
          <div className="flex items-center justify-between z-10 ">
            <p className="text-lg">Total:</p>
            <p className="text-xl text-primary font-bold">AED: 240.00</p>
          </div>
          <Button
            variant={'clip'}
            size={'full'}
            className="uppercase text-lg font-bold z-10 "
          >
            Proceed to Checkout
          </Button>
        </div>
        <Glow className="absolute right-0 bottom-0 w-1/5 h-40 overflow-hidden" />
      </div>

      <div className="py-10 pl-14 pr-4">
        {/* 13 cards */}
        <ProductSection
          class="max-w-sm lg:max-w-sm"
          slidesToShow={3}
          center={false}
          title={'Last chance offer'}
          type="no-glow"
        />
      </div>
      <Glow className="absolute right-0 bottom-0 w-1/5 h-40 overflow-hidden -z-10" />
    </div>
  );
}

type CartItemProp = {
  cartItem: CartItemInterface;
  cart_id: number;
  customer_id: number;
};

type SubscriptionType = 'weekly' | 'monthly' | 'quarterly' | null;

function CartItem(props: CartItemProp) {
  const { cart_id, customer_id, cartItem } = props;
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
    if (!isSubscribe) {
      setIsSubscribe((prevValue) => !prevValue);
      return;
    }
    const quantity =
      type === 'increment'
        ? cartItem?.quantity + 1
        : type === 'decrement'
        ? cartItem?.quantity - 1
        : cartItem?.quantity;

    const isSubscription = type === 'unsubscribe' ? false : isSubscribe;

    const payload = {
      subscription_type: isSubscription ? subscriptionType : null,
      cart_item_id: cartItem?.id,
      customer_id: customer_id,
      cart_id: cart_id,
      event_id: cartItem?.event_id,
      is_subscribe: isSubscription,
      quantity,
    };

    try {
      const response = await addToBasket.mutateAsync(payload);
      console.log({ response });
      dispatch(addToCart(response.data));
      setIsSubscribe(isSubscription);

      toast({
        variant: 'success',
        title: 'Item updated successfully!',
      });

      console.log({ response });
    } catch (error: any) {
      console.log({ error });
    }
  }

  return (
    <div data-name="card" className="card py-8 border-t border-white/40">
      <div className="flex items-start gap-16">
        <div className="relative w-44 h-28">
          <Image
            src={renderNFTImage({ thumb: cartItem.Event.thumb })}
            fill
            alt={'car image'}
            className="absolute object-contain"
          />
          <div className="p-1 w-12 h-12 rounded-full overflow-hidden absolute top-[30%] -right-6 bg-gradient-to-b from-primary to-neutral-900">
            <Image
              src={BottleImage}
              alt={'car image'}
              className="w-full h-full object-cover  rounded-full bg-white"
            />
          </div>
        </div>
        <div className="py-4 flex-1 flex items-start justify-between gap-16">
          <p className="text-2xl flex-1">
            {cartItem?.Event?.EventDescription[0]?.name}
            {/* Win This 800BHP Ferrari E63s Night Edition + AED 1,000 Cash! */}
          </p>
          <div className="flex flex-col w-1/2">
            <div className="flex justify-between items-start ">
              <div className="bg-card flex items-center justify-between overflow-hidden ">
                <Button
                  className="p-2 bg-primary text-background"
                  disabled={cartItem?.quantity === 1 || addToBasket.isLoading}
                  onClick={() => addToBasketHandler('decrement')}
                >
                  <i className="fas fa-minus text-2xl font-extrabold" />
                </Button>
                <p className="w-16 text-center text-xl">{cartItem?.quantity}</p>
                <Button
                  className="p-2 bg-primary text-background"
                  disabled={addToBasket.isLoading}
                  onClick={() => addToBasketHandler('increment')}
                >
                  <i className="fas fa-plus text-2xl font-extrabold" />
                </Button>
              </div>
              <p className="text-xl text-white font-bold">
                AED: {(cartItem?.quantity * cartItem?.Event?.price)?.toFixed(2)}{' '}
              </p>
              <div className="space-y-2">
                <div className="flex items-center gap-3">
                  <p className={isSubscribe ? 'text-white' : 'text-white/40'}>
                    Subscription
                  </p>
                  <Switch
                    disabled={addToBasket.isLoading}
                    onClick={() => addToBasketHandler('unsubscribe')}
                    checked={isSubscribe}
                  />
                </div>
              </div>
            </div>

            {isSubscribe ? (
              <div className="w-1/2 self-end grid grid-cols-2 gap-2">
                {['Weekly', 'Monthly', 'Quarterly'].map((frequency) => (
                  <Button
                    key={frequency}
                    className={`bg-card text-sm rounded-full ${
                      frequency?.toLocaleLowerCase() === subscriptionType
                        ? 'border border-primary'
                        : ''
                    }`}
                    variant="outline"
                    onClick={() =>
                      setSubscriptionType(
                        frequency?.toLocaleLowerCase() as SubscriptionType,
                      )
                    }
                  >
                    {frequency}
                  </Button>
                ))}
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
            className="fas fa-times cursor-pointer text-white/40 text-sm border w-6 h-6 inline-flex items-center justify-center rounded-full"
          />
        </div>
      </div>
      <RemoveItemDialog
        isModal={isModal}
        openChangeHandler={() => setIsModal((preModal) => !preModal)}
        cart_item_id={cartItem.id}
        item_name={cartItem?.Event?.EventDescription[0]?.name ?? ''}
      />
    </div>
  );
}
