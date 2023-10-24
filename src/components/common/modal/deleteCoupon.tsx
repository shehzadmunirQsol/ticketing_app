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
interface SettingDialogInterface {
  selectedItem: any;
  isModal: boolean;
  title: string;
  setTitle: any;
  setSelectedItem: any;
  setIsModal: any;
  refetch: any;
  type: string;
  setType: any;
}
export function CouponDeleteDialog(props: SettingDialogInterface) {
  const { toast } = useToast();
  const [loading, setLoading] = useState<boolean>(false);

  const deleteCoupon: any = trpc.coupon.delete.useMutation({
    onSuccess: () => {
      console.log('upload successfully');
    },
    onError(error: any) {
      console.log({ error });
    },
  });

  const handleClick = async () => {
    try {
      setLoading(true);
      const payload: any = {
        id: props?.selectedItem?.id,
      };
      if (props?.type == 'delete')
        payload.is_deleted = !props?.selectedItem?.is_deleted;
      const data = await deleteCoupon.mutateAsync({ ...payload });

      if (data) {
        setLoading(false);

        props.setIsModal(false);

        toast({
          variant: `${props?.type === 'enabled' ? "disable" : "success"}`,
          title: `${props?.title} ${props?.type === 'enabled' ? 'Disabled' : 'Delete'
            } Successfully`,
        });
        props?.refetch();
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
            <DialogTitle>{props?.title}</DialogTitle>
            <DialogDescription>
              <div className="flex flex-col gap-4 ">
                <div className="  flex gap-2 items-center p-2  ">
                <p>
                    Are You Sure You Want to {props?.type}{' '} this{' '}
                    <span className="text-primary capitalize">
                      {props?.selectedItem?.name}
                    </span>{' '}
                     Coupon
                  </p>
                </div>
              </div>
            </DialogDescription>
          </DialogHeader>
          <DialogFooter>
            <Button type="submit" onClick={() => handleClick()}>
              Yes
            </Button>
          </DialogFooter>
        </DialogContent>
      </Dialog>
      <LoadingDialog open={loading} text={'Deleting coupon...'} />
    </>
  );
}
