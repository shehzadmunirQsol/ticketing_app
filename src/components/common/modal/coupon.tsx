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
export function CouponDialog(props: SettingDialogInterface) {
  const { toast } = useToast();
  const [loading, setLoading] = useState<boolean>(false);

  const bannerUpdate: any = trpc.customer.update.useMutation({
    onSuccess: () => {
      console.log('upload successfully');

      // router.push('/store/wallet-connect');
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
      if (props?.type == 'enabled')
        payload.is_approved = !props?.selectedItem?.is_approved;
      if (props?.type == 'delete')
        payload.is_deleted = !props?.selectedItem?.is_deleted;
      // let data: any;
      const data = await bannerUpdate.mutateAsync({ ...payload });

      if (data) {
        setLoading(false);

        props.setIsModal(false);

        toast({
          variant: 'success',
          title: `${props?.title} ${
            props?.type === 'enabled'
              ? props?.selectedItem?.is_approved
                ? 'disabled'
                : 'Approved'
              : 'deleted'
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
              <div className="flex flex-col gap-4 mt-4">
                <div className="  flex gap-2 items-center p-2  ">
                  Note: By Saving this information customer can apply this
                  coupon on cart.
                </div>
              </div>
            </DialogDescription>
          </DialogHeader>
          <div className=" py-2"></div>
          <DialogFooter>
            <Button type="submit" onClick={() => handleClick()}>
              Save changes
            </Button>
          </DialogFooter>
        </DialogContent>
      </Dialog>
      <LoadingDialog open={loading} text={'Saving data...'} />
    </>
  );
}
