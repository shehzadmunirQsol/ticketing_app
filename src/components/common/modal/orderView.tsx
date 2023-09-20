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

interface OrderViewDialogInterface {
  isModal: boolean;
  isLogin: boolean;
  openChangeHandler: () => void;
  cart_item_id: number;
  event_id: number;
  item_name: string;
}

export function OrderViewDialog(props: OrderViewDialogInterface) {
  const { toast } = useToast();
  const dispatch = useDispatch();

  return (
    <Dialog open={props?.isModal} onOpenChange={props.openChangeHandler}>
      <DialogContent className="">
        <DialogHeader>
          <DialogTitle>Remove Item</DialogTitle>
          <DialogDescription>
            <p className="text-lg">
              Are you sure, you want to remove your cart?
            </p>
          </DialogDescription>
        </DialogHeader>
        <DialogFooter>
          <Button variant={'secondary'} type="button">
            Cancel
          </Button>
          <Button type="submit">Remove</Button>
        </DialogFooter>
      </DialogContent>
    </Dialog>
  );
}
