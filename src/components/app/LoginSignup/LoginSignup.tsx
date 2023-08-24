import React from 'react';
import { Tabs, TabsContent, TabsList, TabsTrigger } from '~/components/ui/tabs';
import { Button } from '@/ui/button';
import {
  Form,
  FormControl,
  FormDescription,
  FormField,
  FormItem,
  FormLabel,
  FormMessage,
} from '@/ui/form';

import { Input } from '@/ui/input';
import { useForm } from 'react-hook-form';
import { useRouter } from 'next/router';
import { FileInput } from '~/components/common/file_input';
import { useEffect, useState } from 'react';
import { trpc } from '~/utils/trpc';
import { getS3ImageUrl } from '~/service/api/s3Url.service';
import { isValidImageType } from '~/utils/helper';
// import { useToast } from '~/components/ui/use-toast';
import SideImage from './SideImage';
import { signupCustomerInput, loginCustomerInput } from '~/schema/customer';
import { useToast } from '~/components/ui/use-toast';

export default function LoginSignup() {
  const toast = useToast();
  const router = useRouter();

  const formSignup = useForm<signupCustomerInput>();
  const formLogin = useForm<loginCustomerInput>();
  // register customer
  const registerCustomer = trpc.customer.register.useMutation({
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

  // register customer
  const loginCustomer = trpc.customer.loginCustomer.useMutation({
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

  // Signup
  const onSubmitSignup = async (values: any) => {
    console.log(values, 'Working');
    const loginResult = await loginCustomer.mutateAsync(values);
    console.log(loginResult, 'loginResult');
  };

  // Login
  const onSubmitLogin = async (values: any) => {
    console.log(values, 'Working');
    const result = await registerCustomer.mutateAsync(values);
    console.log(result, 'result');
  };

  return (
    <div className="mt-24 flex px-6 gap-6 py-6 ">
      <div className="hidden md:block lg:block">
        <SideImage />
      </div>
      <Tabs defaultValue={'login'} className="w-full rounded-none">
        <>
          <TabsList className=" w-full">
            <TabsTrigger value="login" className="w-full ">
              Login
            </TabsTrigger>
            <TabsTrigger value="signup" className="w-full">
              Signup
            </TabsTrigger>
          </TabsList>
        </>
        <TabsContent value="login">
          <Form {...formLogin}>
            <form
              onSubmit={formLogin.handleSubmit(onSubmitLogin)}
              className="justify-center items-center px-8 py-4 space-y-4"
            >
              <div>
                <FormField
                  control={formLogin.control}
                  name="username"
                  render={({ field }) => (
                    <FormItem className="mb-6">
                      <FormLabel className="text-xs text-grayColor">
                        Username or email address*
                      </FormLabel>
                      <FormControl>
                        <Input
                          type="text"
                          placeholder="Enter Username or Email Address"
                          {...field}
                        />
                      </FormControl>
                      <FormMessage />
                    </FormItem>
                  )}
                />
                <FormField
                  control={formLogin.control}
                  name="password"
                  render={({ field }) => (
                    <FormItem className="mb-6">
                      <FormLabel className="text-xs text-grayColor">
                        Password*
                      </FormLabel>
                      <FormControl>
                        <Input
                          type="text"
                          placeholder="Enter Your Password"
                          {...field}
                        />
                      </FormControl>
                      <FormMessage />
                    </FormItem>
                  )}
                />
              </div>
              <div className="flex flex-row justify-end items-center gap-6 ">
                <p className="underline">Forgot Password?</p>
                <Button
                  className=" px-10  text-black font-sans font-[900]   text-xl tracking-[-1px]"
                  variant="clip"
                >
                  LOGIN
                </Button>
              </div>
            </form>
          </Form>
        </TabsContent>
        <TabsContent value="signup">
          <Form {...formSignup}>
            <form
              onSubmit={formSignup.handleSubmit(onSubmitSignup)}
              className="justify-center items-center px-8 py-4 space-y-4"
            >
              <div>
                <FormField
                  control={formSignup.control}
                  name="username"
                  render={({ field }) => (
                    <FormItem className="mb-6">
                      <FormLabel className="text-xs text-grayColor">
                        Username*
                      </FormLabel>
                      <FormControl>
                        <Input
                          type="text"
                          placeholder="Enter Your Username "
                          {...field}
                        />
                      </FormControl>
                      <FormMessage />
                    </FormItem>
                  )}
                />
                <FormField
                  control={formSignup.control}
                  name="email"
                  render={({ field }) => (
                    <FormItem className="mb-6">
                      <FormLabel className="text-xs text-grayColor">
                        Email Address*
                      </FormLabel>
                      <FormControl>
                        <Input
                          type="text"
                          placeholder="Enter Your Email Address"
                          {...field}
                        />
                      </FormControl>
                      <FormMessage />
                    </FormItem>
                  )}
                />
                <FormField
                  control={formSignup.control}
                  name="password"
                  render={({ field }) => (
                    <FormItem className="mb-6">
                      <FormLabel className="text-xs text-grayColor">
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
                  control={formSignup.control}
                  name="firstname"
                  render={({ field }) => (
                    <FormItem className="mb-6">
                      <FormLabel className="text-xs text-grayColor">
                        First Name*
                      </FormLabel>
                      <FormControl>
                        <Input
                          type="text"
                          placeholder="Enter your first name"
                          {...field}
                        />
                      </FormControl>
                      <FormMessage />
                    </FormItem>
                  )}
                />
                <FormField
                  control={formSignup.control}
                  name="lastname"
                  render={({ field }) => (
                    <FormItem className="mb-6">
                      <FormLabel className="text-xs text-grayColor">
                        Last Name*
                      </FormLabel>
                      <FormControl>
                        <Input
                          type="text"
                          placeholder="Enter your last name"
                          {...field}
                        />
                      </FormControl>
                      <FormMessage />
                    </FormItem>
                  )}
                />
              </div>
              <div className="flex flex-row justify-between items-center gap-6 ">
                <p className="text-grayColor text-xs w-96">
                  Your personal data will be used to process your order, support
                  your experience throughout this website, and for other
                  purposes described in our privacy policy.
                </p>
                <Button
                  className=" px-10  text-black font-sans font-[900]   text-xl tracking-[-1px]"
                  variant="clip"
                >
                  LOGIN
                </Button>
              </div>
            </form>
          </Form>
        </TabsContent>
      </Tabs>
    </div>
  );
}

// export default LoginSignup;
