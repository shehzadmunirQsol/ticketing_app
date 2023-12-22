import BottleImage from '~/public/assets/bottle.png';
import { Switch } from '~/components/ui/switch';
import { Button } from '~/components/ui/button';
import { useState } from 'react';
import { trpc } from '~/utils/trpc';
import { useDispatch, useSelector } from 'react-redux';
import { useToast } from '~/components/ui/use-toast';
import { CartItemInterface, addToCart } from '~/store/reducers/cart';
import {
  URIGenerator,
  getAvailableTickets,
  renderNFTImage,
} from '~/utils/helper';
import { RemoveItemDialog } from '~/components/common/modal/cartModal';
import {
  Tooltip,
  TooltipContent,
  TooltipProvider,
  TooltipTrigger,
} from '~/components/ui/tooltip';
import Link from 'next/link';
import { RootState } from '~/store/store';
import NextImage from '~/components/ui/img';

type CartItemProp = {
  cartItem: CartItemInterface;
  cart_id: number;
  customer_id: number;
  ticketPurchased: number;
  cartItemsLength: number;
};

type SubscriptionType = 'weekly' | 'monthly' | 'quarterly' | null;

export default function CartItem(props: CartItemProp) {
  var fullUrl = "";
  if(typeof window !== 'undefined'){
    fullUrl = window.location.protocol + "//" + window.location.host;
  }

  const { cart_id, customer_id, cartItem } = props;
  const { cart } = useSelector((state: RootState) => state.cart);

  const { isLogin, user } = useSelector((state: RootState) => state.auth);
  const [isSubscribe, setIsSubscribe] = useState(cartItem?.is_subscribe);
  const [isModal, setIsModal] = useState(false);
  const [manualCartCount, setManualCartCount] = useState(cartItem?.quantity);

  const [subscriptionType, setSubscriptionType] = useState<SubscriptionType>(
    cartItem?.subscription_type,
  );

  const { toast } = useToast();
  const dispatch = useDispatch();

  const addToBasket = trpc.cart.addToCart.useMutation();

  async function addToBasketHandler(
    type: 'increment' | 'decrement' | 'manualnumber' | 'unsubscribe' | 'update_cart',
    manualCount:any,
  ) {
    let quantity = cartItem.quantity;

    if (type === 'increment') quantity++;
    if (type === 'decrement') quantity--;

    if (type === 'manualnumber'){
      const numericValue: number = parseInt(manualCount);
      quantity = numericValue;
    }

    const isSubscription = type === 'unsubscribe' ? false : isSubscribe;

    const payload = {
      subscription_type: isSubscription ? subscriptionType : null,
      cart_id: cart_id,
      event_id: cartItem?.event_id,
      is_subscribe: isSubscription,
      quantity,
    };

    






    var drawdate = ""; 
    cart?.cartItems?.forEach((event) => { 
      var drwdate = event?.Event?.end_date; 
      if(drwdate){
        var date = drwdate.toISOString().split('T')[0]; 
        var time = drwdate.toTimeString().split(' ')[0];  

        drawdate += date + ' '+ time + ', '; 
      }

    }); 
    drawdate = drawdate.replace(/,\s*$/, "");  





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

      setManualCartCount(quantity);

      toast({
        variant: 'success',
        title: 'Item updated successfully!',
      });

      if ('sendinblue' in window && window?.sendinblue) {


        const eventCartData = cart?.cartItems?.map((event) => ({
          id: event?.event_id,
          price: event?.Event?.price,
          name: event?.Event?.EventDescription[0]?.name,
          quantity: event?.quantity,
          image: renderNFTImage(event?.Event),
        }));
 
        const sendinblue: any = window.sendinblue;
  
        sendinblue?.track(
            'cart_updated',
            {
              "email": user.email,
              "FIRSTNAME": user.first_name
            },
            {
              "data": {
                // "closing_deadline" : drawdate,
                // "cart_expiration_date" : drawdate,
                "url" : fullUrl+"/cart",
                "item" : eventCartData,
              }
            },
            // {
            //   "data": eventCartData
            // },
          ) as any;

        console.log('pushed cart_updated to brevo 3 cart',cart);
        console.log('pushed cart_updated to brevo 3 eventCartData',eventCartData);

      }
    } catch (error: any) {
      console.log({ error });
    }
  }


  function manualCartNumber(e:any) {
     var num = e.target.value;
     setManualCartCount(num);
  }
  function manualCartNumberSet(e:any) {
    var num = e.target.value;
    setManualCartCount(num);
    addToBasketHandler("manualnumber",num)
 }

  function toggleSwitch() {
    if (isSubscribe && subscriptionType) addToBasketHandler('unsubscribe',null);
    else setIsSubscribe((prevValue) => !prevValue);
  }

  const ticketEventPayload = {
    total_tickets: props.cartItem?.Event?.total_tickets,
    tickets_sold: props.cartItem?.Event?.tickets_sold ?? 0,
    user_ticket_limit: props.cartItem?.Event?.user_ticket_limit,
  };

  const { isTicketLimit, isTicketLimitExceeded } = getAvailableTickets({
    event: ticketEventPayload,
    ticketPurchased: props?.ticketPurchased,
    quantity: cartItem?.quantity,
  });

  const categoryRoute = cartItem?.Event?.category_id === 1 ? 'cars' : 'cash';

  const isDateEnded = cartItem?.Event?.end_date
    ? Date.now() > new Date(cartItem?.Event?.end_date)?.getTime()
    : false;

  const isNotEnabled = !cartItem?.Event?.is_enabled;

  let tooltipMessage = '';

  if (isDateEnded)
    tooltipMessage = "Competition is closed, can't proceed to checkout!";
  else if (isNotEnabled)
    tooltipMessage = "Competition is not enabled, can't proceed to checkout!";
  else if (isTicketLimit) tooltipMessage = 'Cannot buy more entries';
  else if (isTicketLimitExceeded)
    tooltipMessage = "Competition closed, can't proceed to checkout!";

  return (
    <div data-name="card" className="py-2 mdx:py-2 border-t border-white/40">
      <div className="mb-2 flex items-center justify-between mdx:hidden">
        <p className="text-xl font-bold">
          {cartItem?.Event?.EventDescription[0]?.name}
        </p>
        <i
          onClick={() => setIsModal((preModal) => !preModal)}
          className="fas fa-trash cursor-pointer text-white/60 text-sm min-w-[24px] min-h-[24px]"
        />
      </div>

      <div className="flex items-center justify-between">
        <div className="relative mr-3 md:mr-10 min-w-[140px] min-h-[90px] mdx:min-w-[176px] mdx:min-h-[112px]">
          <NextImage
            src={renderNFTImage({ thumb: cartItem.Event.thumb })}
            fill
            alt={'car image'}
            className="w-full h-full absolute object-contain"
          />
          {/* <div className="p-1 w-12 h-12 rounded-full overflow-hidden absolute top-[30%] -right-6 bg-gradient-to-b from-primary to-neutral-900">
            <NextImage
              src={BottleImage}
              alt={'car image'}
              className="w-12 h-12 object-cover  rounded-full bg-white"
            />
          </div> */}
        </div>
        <div className="flex-1 flex items-center justify-between space-x-4">
          <Link
            href={`/${categoryRoute}/${URIGenerator(
              cartItem?.Event?.EventDescription[0]?.name ?? '',
              cartItem?.event_id,
            )}`}
            className="hidden flex-1 mdx:block text-lg md:text-xl "
          >
            {cartItem?.Event?.EventDescription[0]?.name}
          </Link>
          <div className="flex flex-wrap flex-col space-y-2">
            <div className="flex justify-between items-center min-w-[200px] md:min-w-[450px] w-1/2 max-w-[550px]">
              <div className="bg-card flex items-center justify-between overflow-hidden ">
                <Button
                  className="p-2 bg-primary text-background"
                  disabled={
                    isDateEnded ||
                    isNotEnabled ||
                    cartItem?.quantity === 1 ||
                    addToBasket.isLoading
                  }
                  onClick={() => addToBasketHandler('decrement',null)}
                >
                  <i className="fas fa-minus text-base xl:text-2xl font-extrabold" />
                </Button>
                {/* <p className="w-16 text-center text-base md:text-lg">{cartItem?.quantity}</p> */}

                <input
                  className="w-16 h-10 text-center text-base md:text-lg bg-card qtyinput"
                  type="number"
                  // value={cartItem?.quantity}
                  value={manualCartCount}
                  onChange={(e) => manualCartNumber(e)}
                  onBlur={(e) => manualCartNumberSet(e)}
                  // onBlur={(e) => { addToBasketHandler("manualnumber",e.target.value) }}
                />

                <TooltipProvider>
                  <Tooltip
                    open={
                      isTicketLimit ||
                      isTicketLimitExceeded ||
                      isDateEnded ||
                      isNotEnabled
                    }
                  >
                    <TooltipTrigger asChild>
                      <Button
                        className="p-2 bg-primary text-background"
                        disabled={
                          isTicketLimit ||
                          isTicketLimitExceeded ||
                          isDateEnded ||
                          isNotEnabled ||
                          addToBasket.isLoading
                        }
                        onClick={() => addToBasketHandler('increment',null)}
                      >
                        <i className="fas fa-plus text-base xl:text-2xl font-extrabold" />
                      </Button>
                    </TooltipTrigger>
                    <TooltipContent>
                      <p className="font-bold">{tooltipMessage}</p>
                    </TooltipContent>
                  </Tooltip>
                </TooltipProvider>
              </div>
              <p className="text-sm md:text-xl text-white font-bold mt-2 md:mt-0 ml-3 md:ml-0">
                <span className="text-sm md:text-base">AED</span> {(cartItem?.quantity * cartItem?.Event?.price)?.toFixed(2)}{' '}
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
                  onClick={() => addToBasketHandler('update_cart',null)}
                >
                  Update Cart
                </Button>
              </div>
            ) : null}
          </div>
          <i
            onClick={() => setIsModal((preModal) => !preModal)}
            className="hidden mdx:inline-flex fas fa-trash cursor-pointer text-white/60 text-base min-w-[24px] min-h-[24px]"
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
        isLast={props?.cartItemsLength === 1}
      />
    </div>
  );
}

const subscription: { [key: string]: number } = {
  Weekly: 7,
  Monthly: 30,
  Quarterly: 90,
};