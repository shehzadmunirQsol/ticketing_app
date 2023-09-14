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
import SideImage from '../../common/SideImage';
import {
  signupCustomerInput,
  loginCustomerInput,
  signupCustomerSchema,
} from '~/schema/customer';
import { useToast } from '~/components/ui/use-toast';
import Link from 'next/link';
import { ForgotPasswordDailog } from './ForgotPassword';
import CarImage from '../../../public/assets/CarLogin.svg';
import { userAuth } from '~/store/reducers/auth';
import { useDispatch } from 'react-redux';
import { OtpVerificationDailog } from './otp-verification';
import { zodResolver } from '@hookform/resolvers/zod';
import { LoadingDialog } from '~/components/common/modal/loadingModal';

export default function LoginSignup() {
  const { toast } = useToast();
  const router = useRouter();
  const dispatch = useDispatch();

  const [isSubmitting, setIsSubmitting] = useState(false);
  // const formSignup = useForm<signupCustomerInput>();

  // 1. Define your form.
  const formSignup = useForm<signupCustomerInput>({
    resolver: zodResolver(signupCustomerSchema),
  });
  const formLogin = useForm<loginCustomerInput>();

  // Handle Forget Password Modal
  const [isModal, setIsModal] = React.useState(false);
  const [otpIsModal, setOtpIsModal] = React.useState(false);
  const [defaultValue, setDefaultValue] = React.useState('login');
  console.log(defaultValue, 'defaultValue');
  // register customer
  const registerCustomer = trpc.customer.register.useMutation({
    onSuccess: (res: any) => {
      const localStorageData = localStorage.setItem(
        'customer',
        JSON.stringify({
          email: res.email,
          id: res.id,
          first_name: res.first_name,
          last_name: res.last_name,
        }),
      );
      console.log(res, 'res');
      toast({
        variant: 'success',
        title: 'User Register Successfully Please Check your Email',
      });
      // router.push('/login')
      setDefaultValue('login');
      formSignup.setValue('username', '');
      formSignup.setValue('email', '');
      formSignup.setValue('password', '');
      formSignup.setValue('firstname', '');
      formSignup.setValue('lastname', '');
      setIsSubmitting(false);
    },
    onError: (err) => {
      console.log(err.message, 'err');
      toast({
        variant: 'destructive',
        title: err.message,
      });
    },
  });

  // login customer
  const loginCustomer = trpc.customer.loginCustomer.useMutation({
    onSuccess: (response) => {
      toast({
        variant: 'success',
        title: 'User Login Successfully ',
      });
      dispatch(userAuth(response?.user));

      router.back();
      formLogin.setValue('user', '');
      formLogin.setValue('password', '');
      router.push('/');
    },
    onError: (err) => {
      console.log(err.message, 'err');
    },
  });

  // Signup
  const onSubmitSignup = async (values: any) => {
    try {
      setIsSubmitting(true);
      console.log(values, 'Working');
      const signupResult = await registerCustomer.mutateAsync(values);
      setOtpIsModal(true);
      setIsSubmitting(false);

      console.log(signupResult, 'signupResult');
    } catch (e: any) {
      setIsSubmitting(false);
      setOtpIsModal(false);
      toast({
        variant: 'destructive',
        title: e.message,
      });
    }
  };

  // Login

  const onSubmitLogin = async (values: any) => {
    if (!values.user && !values.password) {
      toast({
        variant: "destructive",
        title: "Please Enter Your Login Information"
      })
      return;
    } else if (!values.user) {
      toast({
        variant: "destructive",
        title: "Please Enter Your Email"
      })
      return;
    } else if (!values.password) {
      toast({
        variant: "destructive",
        title: "Please Enter Your Password"
      })
      return;
    }

    try {
      setIsSubmitting(true);
      const loginResult = await loginCustomer.mutateAsync(values);
      console.log(loginResult, 'loginResult');
    } catch (e: any) {
      setIsSubmitting(false);
      toast({
        variant: 'destructive',
        title: e.message,
      });
    }
  };

  return (
    <section className="body-font   ">
      <div className="px-5 pt-24 mb-16 lg:pb-0 md:pb-0 lg:py-24 md:py-24 mx-auto flex flex-col-reverse lg:flex-row md:flex-row gap-14 mt-6  w-full max-w-[1300px] ">
        <div className=" lg:w-2/3 md:w-2/3 w-full  h-full  mb-5 lg:mb-0 rounded-lg hidden  lg:block  ">
          <SideImage
            image={CarImage}
            text={'Unlock Your Journey Login or Register for'}
            text2={'Exclusive Access'}
          />
        </div>
        <Tabs
          defaultValue={defaultValue === 'login' ? 'login' : 'signup'}
          className="flex flex-col flex-wrap   lg:w-2/2 md:w-full  lg:text-left  rounded-none border-none  lg:mr-6 bg-card "
        >
          <>
            <TabsList className=" w-full rounded-none border-none ">
              <TabsTrigger
                value="login"
                className="w-full font-black text-md -mt-1 font-sans rounded-none border-none m-0  "
              >
                Login
              </TabsTrigger>
              <TabsTrigger value="signup" className="w-full font-sans text-md font-black">
                Register
              </TabsTrigger>
            </TabsList>
          </>
          <TabsContent value="login">
            <Form {...formLogin} >
              <form
                onSubmit={formLogin.handleSubmit(onSubmitLogin)}
                className="justify-center items-center px-2 lg:px-8 py-4 space-y-4 w-full h-full"
              >
                <div className='w-full'>
                  <FormField
                    control={formLogin.control}
                    name="user"
                    render={({ field }) => (
                      <FormItem className="mb-4">
                        <FormLabel className="text-xs font-thin text-grayColor">
                          Username or email address*
                        </FormLabel>
                        <FormControl>
                          <Input
                            type="text"
                            placeholder="Enter Username or Email Address"
                            {...field}
                            className='rounded-md'
                          />
                        </FormControl>
                        <div className='relative pb-2'>
                          <FormMessage />
                        </div>
                      </FormItem>
                    )}
                  />
                  <FormField
                    control={formLogin.control}
                    name="password"
                    render={({ field }) => (
                      <FormItem className="mb-6">
                        <FormLabel className="text-xs font-thin text-grayColor">
                          Password*
                        </FormLabel>
                        <FormControl>
                          <Input
                            type="password"
                            placeholder="Enter Your Password"
                            {...field}
                            className='rounded-md'
                          />
                        </FormControl>
                        <div className='relative pb-2'>
                          <FormMessage />
                        </div>
                      </FormItem>
                    )}
                  />
                </div>
                <div className="flex  flex-col lg:flex-row md:flex-row  lg:flex justify-end items-center gap-6 ">
                  <p
                    className="underline text-xs lg:text-base md:text-base cursor-pointer"
                    onClick={() => setIsModal(true)}
                  >
                    Forgot Password?
                  </p>
                  <Button
                    className=" px-16 lg:w-40 md:w-40 w-full  text-black font-sans font-[900]   text-xl tracking-[-1px]"
                    variant="clip"
                  >
                    LOGIN
                  </Button>
                </div>
              </form>
            </Form>
          </TabsContent>
          <LoadingDialog open={isSubmitting} text={'Loading...'} />
          <TabsContent value="signup">
            <Form {...formSignup} >
              <form

                onSubmit={formSignup.handleSubmit(onSubmitSignup)}
                className=" justify-center items-center px-2 lg:px-8 py-4 space-y-4 w-full h-full"
              >
                <div className="">
                  <FormField
                    control={formSignup.control}
                    name="username"
                    render={({ field }) => (
                      <FormItem className="mb-4 ">
                        <FormLabel className="text-xs font-thin  text-grayColor">
                          Username*
                        </FormLabel>
                        <FormControl>
                          <Input
                            type="text"
                            placeholder="Enter Your Username "
                            {...field}
                            className='rounded-md'
                          />
                        </FormControl>
                        <div className='relative pb-2'>
                          <FormMessage />
                        </div>
                      </FormItem>
                    )}
                  />
                  <FormField
                    control={formSignup.control}
                    name="email"
                    render={({ field }) => (
                      <FormItem className="mb-4 ">
                        <FormLabel className="text-xs  font-thin text-grayColor">
                          Email Address*
                        </FormLabel>
                        <FormControl>
                          <Input
                            type="text"
                            placeholder="Enter Your Email Address"
                            {...field}
                            className='rounded-md'
                          />
                        </FormControl>
                        <div className='relative pb-2'>
                          <FormMessage />
                        </div>
                      </FormItem>
                    )}
                  />
                  <FormField
                    control={formSignup.control}
                    name="password"
                    render={({ field }) => (
                      <FormItem className="mb-4 ">
                        <FormLabel className="text-xs font-thin text-grayColor">
                          Password*
                        </FormLabel>
                        <FormControl>
                          <Input
                            type="password"
                            placeholder="Enter your password"
                            {...field}
                            className='rounded-md'
                          />
                        </FormControl>
                        <div className='relative pb-2'>
                          <FormMessage />
                        </div>
                      </FormItem>
                    )}
                  />
                  <FormField
                    control={formSignup.control}
                    name="firstname"
                    render={({ field }) => (
                      <FormItem className="mb-4 ">
                        <FormLabel className="text-xs font-thin text-grayColor">
                          First Name*
                        </FormLabel>
                        <FormControl>
                          <Input
                            type="text"
                            placeholder="Enter your first name"
                            {...field}
                            className='rounded-md'
                          />
                        </FormControl>
                        <div className='relative pb-2'>
                          <FormMessage />
                        </div>
                      </FormItem>
                    )}
                  />
                  <FormField
                    control={formSignup.control}
                    name="lastname"
                    render={({ field }) => (
                      <FormItem className="mb-12">
                        <FormLabel className="text-xs font-thin text-grayColor">
                          Last Name*
                        </FormLabel>
                        <FormControl>
                          <Input
                            type="text"
                            placeholder="Enter your last name"
                            {...field}
                            className='rounded-md'
                          />
                        </FormControl>
                        <div className='relative pb-2'>
                          <FormMessage />
                        </div>
                      </FormItem>
                    )}
                  />
                </div>
                <div className="mt-4 flex flex-col lg:flex-row md:flex-row justify-between items-center gap-6 ">
                  <p className="text-lightColor text-gray-400 font-extralight text-xs w-full lg:w-96  md:w-96">
                    Your personal data will be used to process your order,
                    support your experience throughout this website, and for
                    other purposes described in our privacy policy.
                  </p>
                  <Button
                    className="  lg:w-52 md:w-52 w-full     text-black font-sans font-[900]   text-xl tracking-[-1px]"
                    variant="clip"
                  >
                    REGISTER
                  </Button>
                </div>
              </form>
            </Form>
          </TabsContent>
        </Tabs>
        {/* </div> */}
      </div>

      <ForgotPasswordDailog isModal={isModal} setIsModal={setIsModal} />
      <OtpVerificationDailog
        otpIsModal={otpIsModal}
        setOtpIsModal={setOtpIsModal}
      />
    </section>
  );
}

// export default LoginSignup;
