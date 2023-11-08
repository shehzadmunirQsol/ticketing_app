import { useDispatch, useSelector } from 'react-redux';
import { Button } from '~/components/ui/button';
import {
  Dialog,
  DialogContent,
  DialogDescription,
  DialogFooter,
  DialogHeader,
  DialogTitle,
} from '~/components/ui/dialog';
import { useToast } from '~/components/ui/use-toast';
import { addCart, removeFromCart } from '~/store/reducers/cart';
import { RootState } from '~/store/store';
import { trpc } from '~/utils/trpc';

interface SettingDialogInterface {
  isModal: boolean;
  isLogin: boolean;
  isLast: boolean;
  openChangeHandler: () => void;
  cart_item_id: number;
  event_id: number;
  item_name: string;
}

export function RemoveItemDialog(props: SettingDialogInterface) {
  const { cart } = useSelector((state: RootState) => state.cart);

  const { toast } = useToast();
  const dispatch = useDispatch();

  const removeCartItem = trpc.cart.removeFromCart.useMutation();

  async function removeItemHandler() {
    try {
      if (props?.isLogin) {
        const payload = {
          cart_item_id: props?.cart_item_id,
          isLast: props?.isLast,
        };

        await removeCartItem.mutateAsync(payload);
      }
      if (props?.isLast) {
        dispatch(
          addCart({
            id: null,
            customer_id: null,
            isDiscount: false,
            discount: 0,
            isPercentage: false,
            cartItems: [],
          }),
        );

        if ('sendinblue' in window && window?.sendinblue) {
          const sendinblue: any = window.sendinblue;
          sendinblue?.track('cart_deleted' /*mandatory*/) as any;
        }

        localStorage.removeItem('winnar-cart');
      } else {
        const eventCartData = cart?.cartItems
          ?.filter((cartItem) => cartItem.id !== props?.cart_item_id)
          ?.map((cartItem) => ({
            id: cartItem?.event_id,
            price: cartItem?.Event?.price,
            name: cartItem?.Event?.EventDescription[0]?.name,
            quantity: cartItem?.quantity,
          }));

        if ('sendinblue' in window && window?.sendinblue) {
          const sendinblue: any = window.sendinblue;
          sendinblue?.track(
            'cart_updated' /*mandatory*/,
            JSON.stringify({}) /*user data optional*/,
            JSON.stringify({
              cart_id: cart.id,
              data: eventCartData,
            }) /*optional*/,
          );

          console.log('pushed cart_updated to brevo');
        }
      }

      dispatch(removeFromCart({ event_id: props?.event_id }));
      props?.openChangeHandler();
      toast({
        variant: 'success',
        title: 'Item removed successfully!',
      });
    } catch (error: any) {
      props?.openChangeHandler();
      toast({
        variant: 'destructive',
        title: error?.message ?? 'Something went wrong!',
      });
    }
  }

  return (
    <Dialog open={props?.isModal} onOpenChange={props.openChangeHandler}>
      <DialogContent className="">
        <DialogHeader>
          <DialogTitle>Remove Item</DialogTitle>
          <DialogDescription>
            <p className="text-lg">
              Are you sure, you want to remove{' '}
              <strong className="text-primary">{props?.item_name}</strong> from
              your cart?
            </p>
          </DialogDescription>
        </DialogHeader>
        <DialogFooter>
          <Button
            variant={'secondary'}
            type="button"
            onClick={props.openChangeHandler}
            disabled={removeCartItem.isLoading}
          >
            Cancel
          </Button>
          <Button
            type="submit"
            onClick={removeItemHandler}
            disabled={removeCartItem.isLoading}
          >
            Remove
          </Button>
        </DialogFooter>
      </DialogContent>
    </Dialog>
  );
}
