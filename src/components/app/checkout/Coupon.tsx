import { Button } from '~/components/ui/button';
import { Input } from '~/components/ui/input';
import {
  Dialog,
  DialogContent,
  DialogDescription,
  DialogHeader,
  DialogTitle,
} from '~/components/ui/dialog';
import { useToast } from '~/components/ui/use-toast';
import { trpc } from '~/utils/trpc';
import { addDiscount } from '~/store/reducers/cart';
import { useDispatch } from 'react-redux';
import { useState } from 'react';
import { Label } from '~/components/ui/label';

interface ForgotPasswordDialogInterface {
  isModal: boolean;
  setIsModal: (e: any) => void;
  cart_id: number;
  customer_id: number;
}
export function CouponModal(props: ForgotPasswordDialogInterface) {
  const [code, setCode] = useState('');

  const { toast } = useToast();
  const dispatch = useDispatch();

  const couponApply = trpc.coupon.applyCoupon.useMutation({
    onSuccess: () => {
      console.log('upload successfully');

      // router.push('/store/wallet-connect');
    },
    onError(error: any) {
      console.log({ error });
    },
  });

  // Handle Forgot Password

  async function onSubmit(event: React.ChangeEvent<HTMLFormElement>) {
    event.preventDefault();
    try {
      if (!code) return;
      const payload = {
        cart_id: props?.cart_id,
        customer_id: props?.customer_id,
        coupon_code: code,
      };
      const data = await couponApply.mutateAsync(payload);

      const response = {
        isDiscount: true,
        discount: data?.data?.discount ?? 0,
        isPercentage: data?.data?.is_percentage ?? false,
      };

      dispatch(addDiscount(response));
      if (data) {
        toast({
          variant: 'success',
          title: 'Coupon Applied!',
        });
      }
      props?.setIsModal(false);
    } catch (e: any) {
      toast({
        variant: 'destructive',
        title: e.message,
      });
    }
  }

  return (
    <Dialog
      open={props?.isModal}
      onOpenChange={(e: any): any => props.setIsModal(e)}
    >
      <DialogContent className="sm:max-w-[425px]">
        <DialogHeader>
          <DialogTitle className="text-start">Apply Coupon</DialogTitle>
          <DialogDescription>
            <form
              onSubmit={onSubmit}
              className="justify-start  lg:justify-center md:justify-center items-center py-4 space-y-4"
            >
              <div className="mb-6 text-start space-y-2">
                <Label className="text-base text-start font-thin text-grayColor">
                  Coupon Code
                </Label>
                <Input
                  type="text"
                  maxLength={6}
                  placeholder="Enter coupon code"
                  value={code}
                  onChange={(e) => setCode(e.target.value)}
                />
              </div>
              <Button
                className="w-full text-black font-sans font-[900]   text-xl tracking-[-1px]"
                variant="clip"
                disabled={!code || couponApply.isLoading}
              >
                Apply
              </Button>
            </form>
          </DialogDescription>
        </DialogHeader>
      </DialogContent>
    </Dialog>
  );
}
