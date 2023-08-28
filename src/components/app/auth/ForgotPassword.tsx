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
  setIsModal: (e:any) => void;
}
export function ForgotPasswordDailog(props: ForgotPasswordDialogInterface) {
  const { toast } = useToast();
  const router = useRouter();

  // Handle Forgot Password
  const formForgotPassword = useForm<any>();

  // forgot password
  const customerForgotPassword =
    trpc.customer.forgotPasswordCustomer.useMutation({
      onSuccess: async (res: any) => {
        props.setIsModal(false);
      },
      onError: (err) => {
        console.log(err.message, 'err');
      },
    });

  console.log(customerForgotPassword, 'customerForgotPassword');

  const onSubmit = async (values: any) => {
    console.log(values, 'onSubmit');
    const resp = await customerForgotPassword.mutateAsync(values);
    console.log(resp, 'final res');
  };

  return (
    <Dialog open={props?.isModal} onOpenChange={(e) => props.setIsModal(e)}>
      <DialogContent className="sm:max-w-[425px]">
        <DialogHeader>
          <DialogTitle>Forgot Password</DialogTitle>
          <DialogDescription>
            <Form {...formForgotPassword}>
              <form
                onSubmit={formForgotPassword.handleSubmit(onSubmit)}
                className="justify-center items-center px-2 lg:px-8 py-4 space-y-4"
              >
                <FormField
                  control={formForgotPassword.control}
                  name="email"
                  render={({ field }) => (
                    <FormItem className="mb-6">
                      <FormLabel className="text-xs font-thin text-grayColor">
                        Email*
                      </FormLabel>
                      <FormControl>
                        <Input
                          type="text"
                          placeholder="Enter your email"
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
                  Submit
                </Button>
              </form>
            </Form>
          </DialogDescription>
        </DialogHeader>
      </DialogContent>
    </Dialog>
  );
}
