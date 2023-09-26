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
import { sendEmail } from '~/utils/helper';

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
  paragraph: string;
}
export function CustomerDialog(props: SettingDialogInterface) {
  const { toast } = useToast();

  const customerUpdate = trpc.customer.update.useMutation({
    onSuccess: () => {
      console.log('upload successfully');

    },
    onError(error: any) {
      console.log({ error });
    },
  });

  const handleClick = async () => {
    try {
      type Payload = {
        id: number;
        is_deleted?: boolean;
        is_disabled?: boolean;
        type?: string;
      }

      const payload: Payload = {
        id: props?.selectedItem?.id,
        type: props?.type
      };
      if (props?.type == 'delete')
        payload.is_deleted = !props?.selectedItem?.is_deleted;
      if (props?.type == 'enable')
        payload.is_disabled = false;

      // let data: any;
      await customerUpdate.mutateAsync(payload);

      toast({
        variant: `${props?.type === 'enable' ? "success" : 'disable'}`,
        title: `${props?.title} ${props?.type === 'enable' ? "Enabled" : 'Deleted'
          } Successfully`,
      });

      props.setIsModal(false);
      props?.refetch();
    } catch (e: any) {

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
            <DialogTitle className='capitalize'>{`${props?.type} ${props?.title}`}</DialogTitle>
            <DialogDescription>
              <div className="flex flex-col gap-4  ">
                <div className="  flex  items-center py-2  ">
                  {props?.paragraph}
                </div>
              </div>
            </DialogDescription>
          </DialogHeader>

          <DialogFooter>
            <Button type="button" onClick={() => props.setIsModal(false)} variant={"secondary"}>
              Cancel
            </Button>
            <Button type="submit" onClick={() => handleClick()}>
              Save changes
            </Button>
          </DialogFooter>
        </DialogContent>
      </Dialog>
      <LoadingDialog open={customerUpdate.isLoading} text={'Saving data...'} />
    </>
  );
}
