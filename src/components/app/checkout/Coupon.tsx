import { useRouter } from 'next/router';
import { Button } from '~/components/ui/button';
import { Input } from '~/components/ui/input';
import {
  Dialog,
  DialogContent,
  DialogDescription,
  DialogFooter,
  DialogHeader,
  DialogTitle,
  DialogTrigger,
} from '~/components/ui/dialog';
import {
  Form,
  FormControl,
  FormDescription,
  FormField,
  FormItem,
  FormLabel,
  FormMessage,
} from '@/ui/form';
import { useForm } from 'react-hook-form';
import { useToast } from '~/components/ui/use-toast';
import { trpc } from '~/utils/trpc';

interface ForgotPasswordDialogInterface {
  isModal: boolean;
  setIsModal: (e: any) => void;
}
export function CouponModal(props: ForgotPasswordDialogInterface) {
  const { toast } = useToast();
  const router = useRouter();

  // Handle Forgot Password
  const form = useForm<any>();

  const onSubmit = async (values: any) => {
    console.log(values, 'onSubmit');
  };

  return (
    <Dialog
      open={props?.isModal}
      onOpenChange={(e: any): any => props.setIsModal(e)}
    >
      <DialogContent className="sm:max-w-[425px]">
        <DialogHeader>
          <DialogTitle className='text-start'>Apply Coupon</DialogTitle>
          <DialogDescription>
            <Form {...form}>
              <form
                onSubmit={form.handleSubmit(onSubmit)}
                className="justify-start  lg:justify-center md:justify-center items-center px-2 lg:px-8 py-4 space-y-4"
              >
                <FormField
                  control={form.control}
                  name="coupon"
                  render={({ field }) => (
                    <FormItem className="mb-6 text-start">
                      <FormLabel className="text-xs text-start font-thin text-grayColor">
                        Coupon Code
                      </FormLabel>
                      <FormControl>
                        <Input
                          type="text"
                          maxLength={6}
                          placeholder="Enter coupon code"
                          {...field}
                        />
                      </FormControl>
                      <FormMessage />
                    </FormItem>
                  )}
                />
                <Button
                  className="w-full     text-black font-sans font-[900]   text-xl tracking-[-1px]"
                  variant="clip"
                >
                  Apply
                </Button>
              </form>
            </Form>
          </DialogDescription>
        </DialogHeader>
      </DialogContent>
    </Dialog>
  );
}
