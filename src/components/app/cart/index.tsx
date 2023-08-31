import { Button } from '~/components/ui/button';
import { useState } from 'react';
import { Input } from '~/components/ui/input';
import ProductSection from '../home/product_section';
import Glow from '~/components/common/glow';
import { trpc } from '~/utils/trpc';
import { RootState } from '~/store/store';
import { useDispatch, useSelector } from 'react-redux';
import { useToast } from '~/components/ui/use-toast';
import Link from 'next/link';
import CartItem from './cartItem';
import { addDiscount } from '~/store/reducers/cart';

export default function CartPage() {
  const { cart, totalAmount } = useSelector((state: RootState) => state.cart);
  const [code, setCode] = useState('');
  const { toast } = useToast();
  const dispatch = useDispatch();

  const couponApply = trpc.coupon.applyCoupon.useMutation({
    onSuccess: () => {
      console.log('upload successfully');

      // router.push('/store/wallet-connect');
    },
    onError(error: any) {
      console.log({ error });
    },
  });

  async function handleApply() {
    try {
      if (!code) return;
      const payload = {
        cart_id: cart?.id ?? 0,
        customer_id: cart?.customer_id ?? 0,
        coupon_code: code,
      };
      const data = await couponApply.mutateAsync(payload);

      const response = {
        isDiscount: true,
        discount: data?.data?.discount ?? 0,
        isPercentage: data?.data?.is_percentage ?? false,
      };

      dispatch(addDiscount(response));
      if (data) {
        toast({
          variant: 'success',
          title: 'Coupon Applied!',
        });
      }
    } catch (e: any) {
      toast({
        variant: 'destructive',
        title: e.message,
      });
    }
  }

  const discountAmount = cart.isPercentage
    ? totalAmount * (cart.discount / 100)
    : cart.discount;

  return (
    <div className="relative mt-24">
      <div className="relative bg-background py-6 px-4 space-y-10 md:py-16 md:px-14 md:space-y-14">
        <h2 className="text-2xl md:text-4xl lg:text-5xl font-black uppercase">
          Basket
        </h2>
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
              <div className="h-[1px] bg-white/40" />
            </>
          ) : null}

          <div className="flex items-center justify-between z-10 ">
            <p className="text-lg">Total:</p>
            <p className="text-xl text-primary font-bold">
              AED {(totalAmount - discountAmount)?.toFixed(2)}
            </p>
          </div>
          <Link className="block" href="/checkout">
            <Button
              variant={'clip'}
              size={'full'}
              className="uppercase text-lg font-bold z-10 "
            >
              Proceed to Checkout
            </Button>
          </Link>
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
