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
import { Input } from '~/components/ui/input';
interface ForgotPasswordDialogInterface {
  isModal: boolean;
  setIsModal: () => void;
}
export function ForgotPasswordDailog(props: ForgotPasswordDialogInterface) {
  const { toast } = useToast();
  const router = useRouter();

  // Handle Forgot Password
  const formForgotPassword = useForm<any>();

    // register customer
    const customerForgotPassword = trpc.customer.forgotPasswordCustomer.useMutation({
        onSuccess: (res: any) => {
          console.log(res, 'res');
        },
        onError: (err) => {
          console.log(err.message, 'err');
          // toast({
          //   variant: 'success',
          //   title: err.message,
          // });
          // console.log(err.message, 'login err');
        },
      });

      console.log(customerForgotPassword,"customerForgotPassword")
  
  const onSubmit = async (values: any) => {
    console.log(values, 'onSubmit');
    const resp = await customerForgotPassword.mutateAsync(values)
    console.log(resp,"final res")
  };

  return (
    <Dialog open={props?.isModal} onOpenChange={(e:any):any => props.setIsModal(e)}>
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
                  className="  lg:w-52 md:w-52 w-full     text-black font-sans font-[900]   text-xl tracking-[-1px]"
                  variant="clip"
                >
                  Submit
                </Button>
              </form>
            </Form>
          </DialogDescription>
        </DialogHeader>
        <div className="grid gap-4 py-4"></div>
      </DialogContent>
    </Dialog>
  );
}
