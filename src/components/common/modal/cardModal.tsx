import { useState } from 'react';
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
import { LoadingDialog } from './loadingModal';
import { useSelector } from 'react-redux';
import { RootState } from '~/store/store';

interface CardDialogInterface {
  selectedItem: any;
  isModal: boolean;
  setSelectedItem: any;
  setIsModal: any;
  type: string;
  setType: any;
  totalID: string;
  setTotalID: any;
  index: any;
  setIndex: any;
  values: any;
}
export function CardDailog(props: CardDialogInterface) {
  const { toast } = useToast();
  const [loading, setLoading] = useState<boolean>(false);
  const { user } = useSelector((state: RootState) => state.auth);
  const { cart, totalAmount } = useSelector((state: RootState) => state.cart);

  // Update CMS Status
  const deleteCardData = trpc.order.deleteCard.useMutation({
    onError(error: any) {
      console.log(error);
    },
  });
  const checkoutCreator = trpc.order.createCheckout.useMutation({
    onSuccess: () => {
      console.log('upload successfully');
    },
    onError(error: any) {
      console.log({ error });
    },
  });
  const handleClick = async () => {
    try {
      const tempID = props?.totalID;
      setLoading(true);
      props?.setTotalID(null);

      const totalRegID = user?.total_customer_id.split(',');
      const payload: any = {
        checkout_id: tempID,
        registration_id: totalRegID[props?.index],
        customer_id: user?.id,
        total_customer_id: user?.total_customer_id,
        index: props?.index,
      };
      const result = await deleteCardData.mutateAsync(payload);
      const data = await checkoutCreator.mutateAsync({
        values: {
          ...props?.values,
          cart_id: cart?.id,
          customer_id: user?.id,
          total_id: user?.total_customer_id,
        },
      });

      if (result) {
        if (data?.checkout?.data) {
          props?.setTotalID(data?.checkout?.data?.id);
        }
        setLoading(false);
        props.setIsModal(false);
        toast({
          variant: `disable`,
          title: `Card Delete Successfully`,
        });
      } else {
        throw new Error('Data update Error');
      }
    } catch (e: any) {
      setLoading(false);

      props.setIsModal(false);
      toast({
        variant: 'destructive',
        title: e.message,
      });
    }
  };
  return (
    <>
      <Dialog open={props?.isModal} onOpenChange={(e) => props.setIsModal(e)}>
        <DialogContent className="sm:max-w-[425px]">
          <DialogHeader>
            <DialogTitle>Remove Card</DialogTitle>
            <DialogDescription>
              <div className="flex flex-col gap-4 mt-2">
                <div className="  flex gap-2 items-center p-2  ">
                  <p>
                    Are You Sure You Want to{' '}
                    <span className="text-primary capitalize">Remove</span> This
                    Card?
                  </p>
                </div>
              </div>
            </DialogDescription>
          </DialogHeader>
          <DialogFooter>
            <Button
              variant={'secondary'}
              onClick={() => props.setIsModal(false)}
            >
              NO
            </Button>
            <Button type="submit" onClick={() => handleClick()}>
              YES
            </Button>
          </DialogFooter>
        </DialogContent>
      </Dialog>
      <LoadingDialog open={loading} text={'Deleting card...'} />
    </>
  );
}
