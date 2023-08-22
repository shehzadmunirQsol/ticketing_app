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
  selectedItem: object;
  isModal: boolean;
  title: string;
  setTitle: () => void;
  setSelectedItem: () => void;
  setIsModal: () => void;
  refetch: () => void;
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
  const handleClick = async () => {
    try {
      const payload: any = {
        id: props?.selectedItem?.id,
        is_enabled: !props?.selectedItem?.is_enabled,
      };
      console.log('hello from the other side');
      const data = await bannerUpdate.mutateAsync({ ...payload });
      if (data) {
        props.setIsModal(false);

        toast({
          variant: 'success',
          title: `${props?.title} ${
            props?.selectedItem?.is_enabled ? 'disabled' : 'enabled'
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
              props?.selectedItem?.is_enabled ? 'disable' : 'enable'
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
