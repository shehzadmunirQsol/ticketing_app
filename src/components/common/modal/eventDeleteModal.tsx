import { useRouter } from 'next/router';
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

interface deleteEventDialogInterface {
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

export function EventDeleteDialog(props: deleteEventDialogInterface) {
    console.log(props.selectedItem.name,"selectedItem")
    const { toast } = useToast();
    const [loading, setLoading] = useState<boolean>(false);
  
    const deleteEvent: any = trpc.event.delete.useMutation({
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
        const data = await deleteEvent.mutateAsync({ ...payload });
  
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
              <DialogTitle className='uppercase'>{props?.title}</DialogTitle>
              <DialogDescription>
                <div className="flex flex-col gap-4">
                  <div className="  flex gap-2 items-center p-2  ">
                  <p>
                    Are You Sure You Want to {props?.type}{' '} 
                    <span className="text-primary capitalize">
                      {props?.selectedItem?.name}
                    </span>{' '}
                     Product
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
        <LoadingDialog open={loading} text={'Deleting Product...'} />
      </>
    );
  }
  