import { Dispatch, SetStateAction, useState } from 'react';
import { Button } from '~/components/ui/button';
import {
  Dialog,
  DialogContent,
  DialogDescription,
  DialogFooter,
  DialogHeader,
  DialogTitle,
} from '~/components/ui/dialog';
import { ScrollArea, ScrollBar } from '~/components/ui/scroll-area';
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
        id: props?.selectedItem.id,
      };
      if (props?.type == 'enable' || props?.type == 'disable')
        payload.is_enabled = !props?.selectedItem?.is_enabled;

      if (props?.type == 'delete')
        payload.is_deleted = !props?.selectedItem?.is_deleted;

      const result = await updateCmsStatusData.mutateAsync({ ...payload });

      if (result) {
        setLoading(false);
        props.setIsModal(false);
        toast({
          variant: `${
            props?.type === 'enable' || props?.type === 'disable'
              ? props?.selectedItem?.is_enabled
                ? 'disable'
                : 'success'
              : 'success'
          }`,
          title: `${props?.title} ${
            props?.type === 'enable' || props?.type === 'disable'
              ? props?.selectedItem?.is_enabled
                ? 'Disabled'
                : 'Enabled'
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
        <DialogContent className="p-0">
        <ScrollArea className="w-full max-w-[700px] h-[calc(100vh-50px)] max-h-[440px] md:max-h-[530px] scroll-hide">
          <ScrollBar orientation="vertical"></ScrollBar>
          <DialogHeader>
            <DialogTitle className="text-left">{props?.title}</DialogTitle>
            <DialogDescription>
              <div className="flex flex-col gap-4">
                <div className="  flex gap-2 items-center p-2  ">
                  <p>
                    Are You Sure You Want to {props?.type}{' '}
                    <span className="text-primary capitalize">
                      {props?.selectedItem?.CMSDescription?.length
                        ? props?.selectedItem?.CMSDescription[0]?.title
                        : ''}
                      {/* {props?.type} */}
                    </span>{' '}
                    Page?
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
          </ScrollArea>
        </DialogContent>
      </Dialog>
      <LoadingDialog open={loading} text={'Saving data...'} />
    </>
  );
}

type ViewContentDialogTypes = {
  setType: Dispatch<SetStateAction<'terms-condition' | 'privacy-policy' | ''>>;
  type: 'terms-condition' | 'privacy-policy' | '';
};

export function ViewContentDialog(props: ViewContentDialogTypes) {
  const isEnabled = props?.type?.length > 0;

  const { data, isFetching } = trpc.cms.getOneContent.useQuery(
    { type: props?.type },
    { enabled: isEnabled },
  );

  return (
    <>
      <Dialog
        open={props?.type?.length > 0}
        onOpenChange={() => props.setType('')}
      >
        <DialogContent className="max-h-[50vh] h-[calc(100%-200px)] max-w-xl md:max-w-[768px] overflow-y-hidden scroll-hide p-2">
        <ScrollArea className="h-100">
          <ScrollBar orientation="vertical"></ScrollBar>
          <DialogHeader>
            <DialogDescription>
              <div
                className=" cmsStyle p-4"
                dangerouslySetInnerHTML={
                  {
                    __html:
                      data?.data?.CMSDescription[0]?.content?.toString() ??
                      'HTML CONTENT NOT FOUND',
                  } as any
                }
              />
            </DialogDescription>
          </DialogHeader>
        </ScrollArea>
        </DialogContent>
      </Dialog>
      <LoadingDialog open={isFetching} text={'loading...'} />
    </>
  );
}
