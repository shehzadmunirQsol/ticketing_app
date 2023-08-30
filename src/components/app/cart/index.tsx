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
  const { toast } = useToast();

  const { cart, totalAmount } = useSelector((state: RootState) => state.cart);
  const [code, setCode] = useState('');

  const couponApply = trpc.coupon.applyCoupon.useMutation({
    onSuccess: () => {
      console.log('upload successfully');

      // router.push('/store/wallet-connect');
    },
    onError(error: any) {
      console.log({ error });
    },
  });
  const handleApply = async () => {
    try {
      if (code !== '') {
        const payload = {
          cart_id: cart?.id ?? 0,
          customer_id: cart?.customer_id ?? 0,
          coupon_code: code,
        };
        const data = await couponApply.mutateAsync(payload);
        if (data) {
          toast({
            variant: 'success',
            title: 'Coupon Applied!',
          });
        }
      }
    } catch (e: any) {
      toast({
        variant: 'destructive',
        title: e.message,
      });
    }
  };
  console.log({ cart });
  const discountAmount = cart.isPercentage
    ? totalAmount * (cart.discount / 100)
    : cart.discount;

  return (
    <div className="relative">
      <div className="pt-24"></div>
      <div className="relative bg-background py-6 px-4 space-y-10 md:py-16 md:px-14 md:space-y-14">
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
              type="text"
              onChange={(e: any) => setCode(e.target.value)}
              disabled={cart.isDiscount}
              className="px-4 flex-1 bg-transparent border-none z-10 "
            />
            <Button
              variant={'ghost'}
              onClick={handleApply}
              disabled={!code || cart.isDiscount}
              className="text-primary border-l border-border z-10 "
            >
              {cart.isDiscount ? 'Coupon Applied' : 'Apply Coupon'}
            </Button>
          </div>
          {cart.isDiscount ? (
            <>
              <div className="flex items-center justify-between z-10 ">
                <p className="text-white/40  text-lg">Sub Total:</p>
                <p className="text-xl">AED {totalAmount?.toFixed(2)}</p>
              </div>
              <div className="flex items-center justify-between z-10 ">
                <p className="text-white/40  text-lg">Discount:</p>
                <p className="text-xl"> - AED {discountAmount.toFixed(2)}</p>
              </div>
            </>
          ) : null}
          <div className="h-[1px] bg-white/40" />

          <div className="flex items-center justify-between z-10 ">
            <p className="text-lg">Total:</p>
            <p className="text-xl text-primary font-bold">
              AED {(totalAmount - discountAmount)?.toFixed(2)}
            </p>
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
      setSubscriptionType(isSubscription ? subscriptionType : null);

      toast({
        variant: 'success',
        title: 'Item updated successfully!',
      });

      console.log({ response });
    } catch (error: any) {
      console.log({ error });
    }
  }

  function toggleSwitch() {
    if (isSubscribe && subscriptionType) addToBasketHandler('unsubscribe');
    else setIsSubscribe((prevValue) => !prevValue);
  }

  return (
    <div
      data-name="card"
      className="card py-3 mdx:py-6 border-t border-white/40"
    >
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
        <div className="flex-1 flex items-start justify-between space-x-4">
          <p className="hidden mdx:block text-xl xl:text-2xl ">
            {cartItem?.Event?.EventDescription[0]?.name}
            {/* Win This 800BHP Ferrari E63s Night Edition + AED 1,000 Cash! */}
          </p>
          <div className="flex flex-col space-y-2">
            <div className="flex justify-between items-start min-w-[450px] w-1/2 max-w-[550px]">
              <div className="bg-card flex items-center justify-between overflow-hidden ">
                <Button
                  className="p-2 bg-primary text-background"
                  disabled={cartItem?.quantity === 1 || addToBasket.isLoading}
                  onClick={() => addToBasketHandler('decrement')}
                >
                  <i className="fas fa-minus text-xl xl:text-2xl font-extrabold" />
                </Button>
                <p className="w-16 text-center text-xl">{cartItem?.quantity}</p>
                <Button
                  className="p-2 bg-primary text-background"
                  disabled={addToBasket.isLoading}
                  onClick={() => addToBasketHandler('increment')}
                >
                  <i className="fas fa-plus text-xl xl:text-2xl font-extrabold" />
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
                    onClick={toggleSwitch}
                    checked={isSubscribe}
                  />
                </div>
              </div>
            </div>

            {isSubscribe ? (
              <div className="self-end grid grid-cols-2 gap-2">
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
            className="hidden mdx:inline-flex fas fa-times cursor-pointer text-white/40 text-sm border min-w-[24px] min-h-[24px]  items-center justify-center rounded-full"
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
