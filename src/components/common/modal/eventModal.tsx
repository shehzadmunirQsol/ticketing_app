import { useRouter } from 'next/router';
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
import { trpc } from '~/utils/trpc';

interface SelectCustomerInterface {
  isModal: boolean;
  openChangeHandler: () => void;
  event_id: number;
  customer_id: number;
  customer_name: string;
  event_name: string;
}

export function SelectWinnerDialog(props: SelectCustomerInterface) {
  const { toast } = useToast();
  const router = useRouter();

  const selectWinner = trpc.winner.selectWinner.useMutation();

  async function removeItemHandler() {
    try {
      const payload = {
        event_id: props?.event_id,
        customer_id: props?.customer_id,
      };

      await selectWinner.mutateAsync(payload);
      toast({
        variant: 'success',
        title: 'Item removed successfully!',
      });
      router.replace('/admin/winners');
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
          <DialogTitle>Select Winner</DialogTitle>
          <DialogDescription>
            <p className="text-lg">
              Are you sure, you want to select{' '}
              <strong className="text-primary">{props?.customer_name}</strong>{' '}
              as a winner for{' '}
              <strong className="text-primary">{props?.event_name}</strong>{' '}
              event?
            </p>
          </DialogDescription>
        </DialogHeader>
        <DialogFooter>
          <Button
            variant={'secondary'}
            type="button"
            onClick={props.openChangeHandler}
            disabled={selectWinner.isLoading}
          >
            Cancel
          </Button>
          <Button
            type="submit"
            onClick={removeItemHandler}
            disabled={selectWinner.isLoading}
          >
            Select
          </Button>
        </DialogFooter>
      </DialogContent>
    </Dialog>
  );
}
