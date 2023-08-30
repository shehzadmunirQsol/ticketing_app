import { useDispatch } from 'react-redux';
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
import { removeFromCart } from '~/store/reducers/cart';
import { trpc } from '~/utils/trpc';

interface SettingDialogInterface {
  isModal: boolean;
  openChangeHandler: () => void;
  cart_item_id: number;
  item_name: string;
}

export function RemoveItemDialog(props: SettingDialogInterface) {
  const { toast } = useToast();
  const dispatch = useDispatch();

  const removeCartItem = trpc.cart.removeFromCart.useMutation();

  async function removeItemHandler() {
    try {
      const payload = { cart_item_id: props?.cart_item_id };

      await removeCartItem.mutateAsync(payload);
      dispatch(removeFromCart(payload));
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
