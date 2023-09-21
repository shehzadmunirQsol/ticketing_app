import { Button } from '~/components/ui/button';
import { Input } from '~/components/ui/input';
import {
  Dialog,
  DialogContent,
  DialogDescription,
  DialogHeader,
  DialogTitle,
} from '~/components/ui/dialog';
import {
  Form,
  FormControl,
  FormField,
  FormItem,
  FormLabel,
  FormMessage,
} from '@/ui/form';
import { useForm } from 'react-hook-form';
import { useToast } from '~/components/ui/use-toast';
import { trpc } from '~/utils/trpc';
import { LoadingDialog } from '~/components/common/modal/loadingModal';
interface ForgotPasswordDialogInterface {
  isModal: boolean;
  setIsModal: (e: any) => void;
}

export function ForgotPasswordDailog(props: ForgotPasswordDialogInterface) {
  const { toast } = useToast();

  // Handle Forgot Password
  const formForgotPassword = useForm<any>();

  // forgot password
  const customerForgotPassword =
    trpc.customer.forgotPasswordCustomer.useMutation({
      onSuccess: async (res: any) => {
        props.setIsModal(false);
        toast({
          variant: 'success',
          title: 'Please check your email ',
        });
      },
      onError: (err) => {
        console.log(err.message, 'err');
        toast({
          variant: 'destructive',
          title: err.message,
        });
        return;
      },
    });

  const onSubmit = async (values: any) => {
    try {
      const resp = await customerForgotPassword.mutateAsync(values);
      console.log(resp, 'final res');
    } catch (error) {
      console.log(error, 'forgot password');
    }
  };

  return (
    <>
      <Dialog
        open={props?.isModal}
        onOpenChange={(e: any): any => props.setIsModal(e)}
      >
        <DialogContent className="sm:max-w-[425px]">
          <DialogHeader>
            <DialogTitle>Forgot Password</DialogTitle>
            <DialogDescription>
              <Form {...formForgotPassword}>
                <form
                  onSubmit={formForgotPassword.handleSubmit(onSubmit)}
                  className="px-2 lg:px-8 py-4 space-y-4"
                >
                  <FormField
                    control={formForgotPassword.control}
                    name="email"
                    render={({ field }) => (
                      <FormItem className="mb-6">
                        <FormLabel className="text-xs font-thin text-grayColor !text-left">
                          Email*
                        </FormLabel>
                        <FormControl>
                          <Input
                            type="text"
                            placeholder="Enter your email"
                            {...field}
                          />
                        </FormControl>
                        <div className="relative pb-2">
                          <FormMessage />
                        </div>
                      </FormItem>
                    )}
                  />
                  <Button
                    className="w-full text-black font-sans font-[900] text-xl tracking-[-1px]"
                    variant="clip"
                  >
                    SUBMIT
                  </Button>
                </form>
              </Form>
            </DialogDescription>
          </DialogHeader>
        </DialogContent>
      </Dialog>
      <LoadingDialog
        open={customerForgotPassword.isLoading}
        text={'Loading...'}
      />
    </>
  );
}
