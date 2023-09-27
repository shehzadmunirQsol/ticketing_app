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
export function CmsDailog(props: SettingDialogInterface) {
  const { toast } = useToast();
  const [loading, setLoading] = useState<boolean>(false);

  // Update CMS Status
  const updateCmsStatusData = trpc.cms.cmsStatusUpdateById.useMutation({
    onSuccess: (res: any) => {
      console.log(res);
      toast({
        variant: 'success',
        title: 'Status Updated Successfully',
      });
    },
    onError(error: any) {
      console.log(error);
    },
  });

  const handleClick = async () => {
    try {
      setLoading(true);
      const payload: any = {
        id: props?.selectedItem,
      };
      const result = await updateCmsStatusData.mutateAsync(payload);
      console.log(result);

      if (result) {
        setLoading(false);
        props.setIsModal(false);
        toast({
          variant: `${props?.type === 'enable' ? 'success' : 'disable'}`,
          title: `${props?.title} ${
            props?.type === 'enable' ? 'Enabled' : 'Disabled'
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
                  <p>
                    Are You Sure You Want to{' '}
                    <span className="text-primary capitalize">
                      {props?.type}
                    </span>{' '}
                    This Page?
                  </p>
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
