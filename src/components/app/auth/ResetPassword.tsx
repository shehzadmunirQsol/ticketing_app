import { useRouter } from 'next/router';
import { useEffect, useState } from 'react';
import { useToast } from '~/components/ui/use-toast';
import { Button } from '~/components/ui/button';
import { Input } from '~/components/ui/input';
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
import { trpc } from '~/utils/trpc';

export default function ResetPassword() {
  const { toast } = useToast();
  const router = useRouter();
  const query = router.query
  console.log(router.query,"i am router query")

  // Handle Reset Password
  const formResetPassword = useForm<any>();


  // Reset Password Customer
  const customerResetPassword =
    trpc.customer.resetPasswordCustomer.useMutation({
      onSuccess: async (res: any) => {
        console.log(res);
        toast({
          variant: 'success',
          title: 'Reset Password Successfully',
        });
        router.push('/login')
      },  
      onError: (err) => {
        console.log(err.message, 'err');
        toast({
          variant: 'destructive',
          title: err.message,
        });
      },
    });

  const onSubmit = async (values: any) => {
    console.log(values, 'onSubmit');
    const payload:any = {
      email: query.email,
      otp: query.verification_code,
      password:values.password,
      confirmPassword:values.confirmPassword

    };
    console.log(payload,"payload")
    const resp:any = await customerResetPassword.mutateAsync(payload);
    console.log(resp, 'final res');
  };

  return (
    <div className="w-2/3 mt-36 mb-20 mx-auto bg-card py-10 px-10">
      <p className="text-3xl text-primary font-black">Reset Password</p>
      <Form {...formResetPassword}>
        <form
          onSubmit={formResetPassword.handleSubmit(onSubmit)}
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
                    type="text"
                    placeholder="Enter your password"
                    {...field}
                  />
                </FormControl>
                <FormMessage />
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
                    type="text"
                    placeholder="Enter your confirm password"
                    {...field}
                  />
                </FormControl>
                <FormMessage />
              </FormItem>
            )}
          />
          <Button
            className="w-full text-black font-sans font-[900]   text-xl tracking-[-1px]"
            variant="clip"
          >
            Change Password
          </Button>
        </form>
      </Form>
    </div>
  );
}
