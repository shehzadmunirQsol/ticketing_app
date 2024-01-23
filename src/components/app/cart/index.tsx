import { Button } from '~/components/ui/button';
import React, { useEffect, useState } from 'react';
import { Input } from '~/components/ui/input';
import ProductSection from '../home/product_section';
import Glow from '~/components/common/glow';
import { trpc } from '~/utils/trpc';
import { RootState } from '~/store/store';
import { useDispatch, useSelector } from 'react-redux';
import { useToast } from '~/components/ui/use-toast';
import CartItem from './cartItem';
import { addDiscount } from '~/store/reducers/cart';
import langContent from '~/locales';
import { getAvailableTickets } from '~/utils/helper';

export default function CartPage() {

  const { cart, totalAmount, count } = useSelector(
    (state: RootState) => state.cart,
  );
  const { lang } = useSelector((state: RootState) => state.layout);
  const { isLogin } = useSelector((state: RootState) => state.auth);

  const eventIds = cart.cartItems.map((item) => item.event_id);
  const [code, setCode] = useState('');
  const { toast } = useToast();
  const dispatch = useDispatch();

  const { data: userTicketLimits } = trpc.cart.getUserTicketLimit.useQuery(
    {
      event_ids: eventIds,
    },
    {
      refetchOnWindowFocus: false,
      enabled: eventIds.length ? true : false,
    },
  );

  const couponApply = trpc.coupon.applyCoupon.useMutation();

  async function handleApply() {
    if (!isLogin) {
      toast({
        variant: 'disable',
        title: 'Please login to apply this coupon!',
      });
      return;
    }
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

  const discountAmount = cart?.isPercentage
    ? totalAmount * (cart?.discount / 100)
    : cart?.discount;

  const bankLimit = 2500;
  const isBankLimitExeed = totalAmount - discountAmount > bankLimit;

  const isCheckoutDisabled = cart?.cartItems?.some((cartItem) => {
    const userTicketLimit = userTicketLimits?.data?.find(
      (userLimit) => userLimit?.event_id === cartItem?.event_id,
    );
    const ticketPurchased = userTicketLimit?._sum?.quantity ?? 0;

    const ticketEventPayload = {
      total_tickets: cartItem?.Event?.total_tickets,
      tickets_sold: cartItem?.Event?.tickets_sold ?? 0,
      user_ticket_limit: cartItem?.Event?.user_ticket_limit,
    };

    const { isTicketLimitExceeded } = getAvailableTickets({
      event: ticketEventPayload,
      ticketPurchased: ticketPurchased,
      quantity: cartItem?.quantity,
    });

    const isDateEnded = cartItem?.Event?.end_date
      ? Date.now() > new Date(cartItem?.Event?.end_date)?.getTime()
      : false;
    const isNotEnabled = !cartItem?.Event?.is_enabled;



    return isTicketLimitExceeded || isDateEnded || isNotEnabled || isBankLimitExeed;
  });



  return (
    <div className="relative mt-24 z-20">
      {count === 0 ? (
        <h2 className="py-20 md:py-40 lg:py-48 text-center text-2xl md:text-4xl lg:text-5xl font-black uppercase">
          No items in the Cart
        </h2>
      ) : (
        <>
          <div className="relative py-6 px-4 space-y-10 md:py-16 md:px-14 md:space-y-10 -z-30">
            <h2 className="text-2xl md:text-4xl lg:text-5xl z-10 font-black uppercase">
              {langContent[lang.lang].Cart.HEADING} <span className="text-xs md:text-base capitalize font-bold block md:inline-block">({langContent[lang.lang].Cart.MAXAMOUNT} AED {bankLimit}.)</span>
            </h2>
            <div
              data-name="cards"
              className="w-full z-10 border-b border-white/40"
            >

              {cart?.cartItems?.map((cartItem) => {
                const userTicketLimit = userTicketLimits?.data?.find(
                  (userLimit) => userLimit?.event_id === cartItem?.event_id,
                );
                const ticketPurchased = userTicketLimit?._sum?.quantity ?? 0;

                return (
                  <CartItem
                    key={cartItem.id}
                    cartItem={cartItem}
                    cart_id={cart?.id ?? 0}
                    customer_id={cart?.customer_id ?? 0}
                    ticketPurchased={ticketPurchased}
                    cartItemsLength={cart?.cartItems?.length ?? 0}
                    lang={lang ? lang.lang_id: 1}
                  />
                );
              })}
            </div>
            <div className="bg-transparent space-y-4 sm:w-1/2 mdx:w-1/3 z-10 ml-auto">
              <div
                className="flex bg-card border border-border rounded-md"
                dir="ltr"
              >
                <Input
                  placeholder={langContent[lang.lang].Cart.COUPON_CODE}
                  type="text"
                  onChange={(e: any) => setCode(e.target.value)}
                  disabled={cart.isDiscount || couponApply.isLoading}
                  className="px-4 flex-1 bg-transparent border-none z-10 "
                />
                <Button
                  variant={'ghost'}
                  onClick={handleApply}
                  disabled={!code || cart.isDiscount || couponApply.isLoading}
                  className="text-primary border-l border-border z-10 "
                >
                  {cart.isDiscount
                    ? langContent[lang.lang].Cart.COUPON_APPLIED
                    : langContent[lang.lang].Cart.APPLY_COUPON}
                </Button>
              </div>
              {cart.isDiscount ? (
                <>
                  <div className="flex items-center justify-between z-10 ">
                    <p className="text-white/40  text-lg">
                      {langContent[lang.lang].Cart.SUB_TOTAL}:
                    </p>
                    <p className="text-xl">AED {totalAmount?.toFixed(2)}</p>
                  </div>
                  <div className="flex items-center justify-between z-10 ">
                    <p className="text-white/40  text-lg">
                      {langContent[lang.lang].Cart.DISCOUNT}:
                    </p>
                    <p className="text-xl">
                      {' '}
                      - AED {discountAmount.toFixed(2)}
                    </p>
                  </div>
                  <div className="h-[1px] bg-white/40" />
                </>
              ) : null}

              <div className="flex items-center justify-between z-10 ">
                <p className="text-lg">{langContent[lang.lang].Cart.TOTAL}:</p>
                <p className="text-xl text-primary font-black">
                  AED {(totalAmount - discountAmount)?.toFixed(2)}
                </p>
              </div>
              <Button
                disabled={isCheckoutDisabled}
                variant={'clip'}
                size={'full'}
                className="uppercase text-lg font-black z-10"
                onClick={() => (window.location.href = '/checkout')}
              >
                {langContent[lang.lang].Cart.CHECHKOUT_BTN}
              </Button>

              {isBankLimitExeed && (
                <div className="bg-red-100 border border-red-400 text-red-700 rounded relative px-3 py-2 text-xs md:text-sm" role="alert">
                  <span className="block sm:inline">{langContent[lang.lang].Cart.MAXAMOUNTMESSAGE} <strong className="font-bold">AED {bankLimit}.</strong></span>
                </div>
              )}

            </div>
            <Glow className="absolute right-0 -z-10 bottom-0 w-1/6 h-40 overflow-hidden" />
          </div>

          <div className="relative py-4 px-4 md:gap-14 md:px-14 z-10 bg-card-foreground">
            <ProductSection
              class="mx-auto w-3/5 md:w-full"
              slidesToShow={3}
              center={false}
              breakpoint={[3, 2, 1.5]}
              breakpointScreens={[1350, 1050, 800]}
              title={langContent[lang.lang].Cart.LAST_OFFER}
              type="closing"
            />
            <Glow className="absolute right-0 bottom-0 w-1/6 h-20 overflow-hidden -z-20" />
          </div>
        </>
      )}
    </div>
  );
}
