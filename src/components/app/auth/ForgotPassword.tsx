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
import {
  forgotPasswordCustomerSchema,
  forgotPasswordCustomerSchemaInput,
} from '~/schema/customer';
import { zodResolver } from '@hookform/resolvers/zod';
import langContent from '~/locales';
import { useSelector } from 'react-redux';
import { RootState } from '~/store/store';

interface ForgotPasswordDialogInterface {
  isModal: boolean;
  setIsModal: (e: any) => void;
}

export function ForgotPasswordDailog(props: ForgotPasswordDialogInterface) {
  const { lang } = useSelector((state: RootState) => state.layout);

  const { toast } = useToast();

  // Handle Forgot Password
  const formForgotPassword = useForm<forgotPasswordCustomerSchemaInput>({
    resolver: zodResolver(forgotPasswordCustomerSchema),
  });

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
        toast({
          variant: 'destructive',
          title: err.message,
        });
      },
    });

  const onSubmit = async (values: forgotPasswordCustomerSchemaInput) => {
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
        <DialogContent className="sm:max-w-[425px] px-2 lg:px-8">
          <DialogHeader>
            <DialogTitle className=" text-center">
              {langContent[lang.lang].Auth.FORGOTPASSWORD.HEADING}{' '}
            </DialogTitle>
            <DialogDescription>
              <Form {...formForgotPassword}>
                <form
                  onSubmit={formForgotPassword.handleSubmit(onSubmit)}
                  className=" py-4 space-y-4"
                >
                  <FormField
                    control={formForgotPassword.control}
                    name="email"
                    render={({ field }) => (
                      <FormItem className="mb-6 text-left">
                        <FormLabel className="text-xs font-thin text-grayColor ">
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


                  <div className="w-full mx-auto">
                    <div className=" flex items-center justify-center">
                      <Button
                        className="align-center uppercase rounded-full px-10   text-black font-sans font-[900]   text-xl tracking-[-1px]"
                        variant="clip"
                      >
                        {langContent[lang.lang].Auth.FORGOTPASSWORD.BUTTON}
                      </Button>
                    </div>
                  </div>
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
