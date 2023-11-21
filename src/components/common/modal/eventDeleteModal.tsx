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
import { toggleSwitchType, EventDataType } from '../table/events';

interface EventSwitchDialogInterface {
  selectedItem: EventDataType;
  setSelectedItem: any;
  isModal: boolean;
  setIsModal: any;
  refetch: any;
  type: toggleSwitchType;
}

export function EventSwitchDialog(props: EventSwitchDialogInterface) {
  const { toast } = useToast();

  const switchUpdate = trpc.event.switchUpdate.useMutation({
    onSuccess: () => {
      console.log('upload successfully');
    },
    onError(error: any) {
      console.log({ error });
    },
  });

  const handleClick = async () => {
    try {
      const payload = {
        id: props?.selectedItem?.id,
        type: props?.type,
        value: !props?.selectedItem[props?.type],
      };

      const data = await switchUpdate.mutateAsync(payload);

      if (data) {
        toast({
          variant: `${
            props?.selectedItem[props?.type] ? 'disable' : 'success'
          }`,
          title: `${
            typeToTitle[props?.type][props?.selectedItem[props?.type] + '']
              ?.title + 'd'
          } Successfully`,
        });
        props?.refetch();
      } else {
        throw new Error('Data update Error');
      }
    } catch (e: any) {
      toast({
        variant: 'destructive',
        title: e.message,
      });
    }

    props?.setIsModal(false);
    props?.setSelectedItem({});
  };

  return (
    <>
      <Dialog
        open={props?.isModal}
        onOpenChange={(e) => props?.setIsModal(false)}
      >
        <DialogContent className="sm:max-w-[425px]">
          <DialogHeader>
            <DialogTitle className="uppercase">Product</DialogTitle>
            <DialogDescription>
              <div className="flex flex-col gap-4">
                <div className="flex gap-2 items-center p-2  ">
                  <p>
                    Are You Sure You Want to{' '}
                    {
                      typeToTitle[props?.type][
                        props?.selectedItem[props?.type] + ''
                      ]?.title
                    }{' '}
                    <span className="text-primary capitalize">
                      {props?.selectedItem?.name}
                    </span>{' '}
                    Product?
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
      <LoadingDialog
        open={switchUpdate.isLoading}
        text={`${
          typeToTitle[props?.type][props?.selectedItem[props?.type] + '']
            ?.loading
        } Product...`}
      />
    </>
  );
}

const typeToTitle: any = {
  is_deleted: {
    false: {
      title: 'Delete',
      loading: 'Deleting',
    },
  },
  is_enabled: {
    true: {
      title: 'Disable',
      loading: 'Disabling',
    },
    false: {
      title: 'Enable',
      loading: 'Enabling',
    },
  },
  is_featured: {
    true: {
      title: 'Un Feature',
      loading: 'Un Featuring',
    },
    false: {
      title: 'Feature',
      loading: 'Featuring',
    },
  },
};
