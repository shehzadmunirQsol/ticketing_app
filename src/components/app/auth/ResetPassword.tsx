import { useRouter } from 'next/router';
import { useEffect, useState } from 'react';
import { useToast } from '~/components/ui/use-toast';
import { Button } from '~/components/ui/button';
import { Input } from '~/components/ui/input';
import {
  Form,
  FormControl,
  FormField,
  FormItem,
  FormLabel,
  FormMessage,
} from '@/ui/form';
import { useForm } from 'react-hook-form';
import { trpc } from '~/utils/trpc';
import { LoadingDialog } from '~/components/common/modal/loadingModal';
import langContent from '~/locales';
import { useSelector } from 'react-redux';
import { RootState } from '~/store/store';

export default function ResetPassword() {
  const { lang } = useSelector((state: RootState) => state.layout);

  const { toast } = useToast();
  const router = useRouter();
  const [userDetail, setUserDetail] = useState<any>(null);

  // Handle Reset Password
  const formResetPassword = useForm<any>();
  useEffect(() => {
    if (router.query) {
      setUserDetail({ ...router.query });
    }
  }, []);
  console.log({ userDetail });

  // Reset Password Customer
  const customerResetPassword = trpc.customer.resetPasswordCustomer.useMutation(
    {
      onSuccess: async () => {
        toast({
          variant: 'success',
          title: 'Reset Password Successfully',
        });
        router.push('/login');
      },
      onError: (err) => {
        toast({
          variant: 'destructive',
          title: err.message,
        });
      },
    },
  );

  const onSubmit = async (values: any) => {
    console.log(values, 'onSubmit');
    if (values.confirmPassword !== values?.password) {
      toast({
        variant: 'destructive',
        title: 'Password are not matching ',
      });
    } else {
      const payload: any = {
        email: userDetail && (userDetail.email as string),
        otp: userDetail && (userDetail.verification_code as string),
        password: values.password,
        confirmPassword: values.confirmPassword,
      };
      await customerResetPassword.mutateAsync(payload);
    }
  };

  return (
    <div className="w-full  lg:w-2/3 md:w-2/3  mt-36 mb-20 mx-auto bg-card py-10 px-5 lg:px-10 md:px-10 max-w-[1300px]">
      <p className="text-3xl text-primary font-black px-2 lg:px-8 uppercase">
        {langContent[lang.lang].Auth.RESETPASSWORD.HEADING}
      </p>
      <Form {...formResetPassword}>
        <form
          onSubmit={formResetPassword.handleSubmit(onSubmit)}
          dir="ltr"
          className="justify-center items-center px-2 lg:px-8 py-4 space-y-4"
        >
          <FormField
            control={formResetPassword.control}
            name="password"
            render={({ field }) => (
              <FormItem className="mb-6">
                <FormLabel className="text-xs font-thin text-grayColor">
                  Password*
                </FormLabel>
                <FormControl>
                  <Input
                    type="password"
                    placeholder="Enter your password"
                    {...field}
                  />
                </FormControl>
                <div className="relative pb-2 errormsg">
                  <FormMessage />
                </div>
              </FormItem>
            )}
          />
          <FormField
            control={formResetPassword.control}
            name="confirmPassword"
            render={({ field }) => (
              <FormItem className="mb-6">
                <FormLabel className="text-xs font-thin text-grayColor">
                  Confirm Password*
                </FormLabel>
                <FormControl>
                  <Input
                    type="password"
                    placeholder="Enter your confirm password"
                    {...field}
                  />
                </FormControl>
                <div className="relative pb-2 errormsg">
                  <FormMessage />
                </div>
              </FormItem>
            )}
          />
          <Button
            className="w-full text-black font-sans font-[900]   text-xl tracking-[-1px] uppercase"
            variant="clip"
          >
            {langContent[lang.lang].Auth.RESETPASSWORD.BUTTON}
          </Button>
        </form>
      </Form>
      <LoadingDialog
        open={customerResetPassword.isLoading}
        text={'Loading...'}
      />
    </div>
  );
}
