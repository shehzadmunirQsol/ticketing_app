import { useRouter } from 'next/router';
import { Button } from '~/components/ui/button';
import {
  Dialog,
  DialogContent,
  DialogDescription,
  DialogFooter,
  DialogHeader,
  DialogTitle,
  DialogTrigger,
} from '~/components/ui/dialog';
import { useToast } from '~/components/ui/use-toast';
import { trpc } from '~/utils/trpc';
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
export function SettingDialog(props: SettingDialogInterface) {
  const { toast } = useToast();
  const router = useRouter();

  const bannerUpdate = trpc.settings.banner_update.useMutation({
    onSuccess: () => {
      console.log('upload successfully');

      // router.push('/store/wallet-connect');
    },
    onError(error: any) {
      console.log({ error });
    },
  });
  const bannerDelete = trpc.settings.banner_delete.useMutation({
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
      const payload: any = {
        id: props?.selectedItem?.id,
      };
      if (props?.type == 'enabled')
        payload.is_enabled = !props?.selectedItem?.is_enabled;
      if (props?.type == 'delete')
        payload.is_deleted = !props?.selectedItem?.is_deleted;
      let data: any;
      if (props?.type == 'delete') {
        data = await bannerDelete.mutateAsync({ ...payload });
      } else {
        data = await bannerUpdate.mutateAsync({ ...payload });
      }
      if (data) {
        props.setIsModal(false);

        toast({
          variant: 'success',
          title: `${props?.title} ${
            props?.type === 'enabled'
              ? props?.selectedItem?.is_enabled
                ? 'disabled'
                : 'enabled'
              : 'deleted'
          } Successfully`,
        });
        props?.refetch();
      } else {
        throw new Error('Data update Error');
      }
    } catch (e: any) {
      props.setIsModal(false);
      toast({
        variant: 'destructive',
        title: e.message,
      });
    }
  };
  return (
    <Dialog open={props?.isModal} onOpenChange={(e) => props.setIsModal(e)}>
      <DialogContent className="sm:max-w-[425px]">
        <DialogHeader>
          <DialogTitle>{props?.title}</DialogTitle>
          <DialogDescription>
            {`Are You Sure You want to ${
              props?.type === 'enabled'
                ? props?.selectedItem?.is_enabled
                  ? 'disable'
                  : 'enable'
                : 'delete'
            } this ${props?.title}`}
          </DialogDescription>
        </DialogHeader>
        <div className="grid gap-4 py-4"></div>
        <DialogFooter>
          <Button type="submit" onClick={() => handleClick()}>
            Save changes
          </Button>
        </DialogFooter>
      </DialogContent>
    </Dialog>
  );
}
