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
import { formatTrpcError } from '~/utils/helper';
interface addressDialogInterface {
  isModal: boolean;
  title: string;
  setIsModal: any;
  type: string;
  openChangeHandler: () => void;
  refetch: () => void;
  customer_id: number;
  address_type?: 'home' | 'work' | 'hotel' | 'other';
  id?: number;
  street_address_1?: string | null;
  street_address_2?: string | null;
  country?: string | null;
  state?: string | null;
  city?: string | null;
  phone_number?: string | null;
  phone_code?: string | null;
  postal_code?: number | null;
}
export function AddressDialog(props: addressDialogInterface) {
  const { toast } = useToast();
  const [loading, setLoading] = useState<boolean>(false);

  const updateAddress = trpc.customer.updateDefaultAddress.useMutation({
    onSuccess: (res) => {
      console.log(res);
      toast({
        variant: 'success',
        title: 'Address Updated Successfully',
      });
      setLoading(false);
    },
    onError(error) {
      const errorMessage = formatTrpcError(error?.shape?.message);
      setLoading(false);

      toast({
        variant: 'destructive',
        title: errorMessage,
      });
    },
  });

  const handleClick = async () => {
    try {
      setLoading(true);
      const payload = { customer_id: props?.customer_id, id: props?.id };
      await updateAddress.mutateAsync({ ...payload });

      toast({
        variant: 'success',
        title: 'Winner Selected successfully!',
      });
      props?.openChangeHandler();
      props?.refetch();
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
                    Note: Do you want save your{' '}
                    <span className="text-primary">{props?.type}</span> address
                    as default?
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
      <LoadingDialog open={loading} text={'Saving data...'} />
    </>
  );
}
