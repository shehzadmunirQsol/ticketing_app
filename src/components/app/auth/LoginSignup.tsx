import React from 'react';
import { Tabs, TabsContent, TabsList, TabsTrigger } from '~/components/ui/tabs';
import { Button } from '@/ui/button';
import {
  Form,
  FormControl,
  FormField,
  FormItem,
  FormLabel,
  FormMessage,
} from '@/ui/form';
import { Input } from '@/ui/input';
import { useForm } from 'react-hook-form';
import { useRouter } from 'next/router';
import { useState } from 'react';
import { trpc } from '~/utils/trpc';
import SideImage from '../../common/SideImage';
import {
  signupCustomerInput,
  loginCustomerInput,
  loginCustomerSchema,
  signupCustomerSchemaInput,
} from '~/schema/customer';
import { useToast } from '~/components/ui/use-toast';
import Link from 'next/link';
import { ForgotPasswordDailog } from './ForgotPassword';
import CarImage from '../../../public/assets/CarLogin.svg';
import { userAuth } from '~/store/reducers/auth';
import { useDispatch, useSelector } from 'react-redux';
import { OtpVerificationDailog } from './otp-verification';
import { zodResolver } from '@hookform/resolvers/zod';
import { LoadingDialog } from '~/components/common/modal/loadingModal';
import {
  Select,
  SelectContent,
  SelectGroup,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from '~/components/ui/select';
import langContent from '~/locales';
import { RootState } from '~/store/store';

import countryJSON from '~/data/countries.json';
const countries = countryJSON.map((item) => item.country);

export default function LoginSignup() {
  const { lang } = useSelector((state: RootState) => state.layout);

  const { toast } = useToast();
  const router = useRouter();
  const dispatch = useDispatch();

  const [user, setUser] = useState(null);

  // 1. Define your form.
  const formSignup = useForm<signupCustomerInput>({
    resolver: zodResolver(signupCustomerSchemaInput),
    defaultValues: {
      code: '',
    },
  });
  const formLogin = useForm<loginCustomerInput>({
    resolver: zodResolver(loginCustomerSchema),
  });

  // Handle Forget Password Modal
  const [isModal, setIsModal] = React.useState(false);
  const [otpIsModal, setOtpIsModal] = React.useState(false);
  const [defaultValue, setDefaultValue] = React.useState('login');

  // register customer
  const registerCustomer = trpc.customer.register.useMutation({
    onSuccess: (res: any) => {
      setUser(res.email);
      localStorage.setItem(
        'customer',
        JSON.stringify({
          email: res.email,
          id: res.id,
          first_name: res.first_name,
          last_name: res.last_name,
        }),
      );
      toast({
        variant: 'success',
        title: 'Registeration Successful, Please Check your Email',
      });
      setDefaultValue('login');
      formSignup.reset();
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
  const loginCustomer = trpc.customer.loginCustomer.useMutation();

  // Signup
  const onSubmitSignup = async (values: any) => {
    try {
      const payload = { ...values, email: values.email.toLowerCase().trim() };
      formLogin.reset();
      await registerCustomer.mutateAsync(payload);
      setOtpIsModal(true);
    } catch (e: any) {
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
        variant: 'destructive',
        title: 'Please Enter Your Login Information',
      });
      return;
    } else if (!values.user) {
      toast({
        variant: 'destructive',
        title: 'Please Enter Your Email',
      });
      return;
    } else if (!values.password) {
      toast({
        variant: 'destructive',
        title: 'Please Enter Your Password',
      });
      return;
    }

    try {
      const payload = { ...values, user: values.user.toLowerCase().trim() };

      formSignup.reset();
      const loginResult = await loginCustomer.mutateAsync(payload);

      dispatch(userAuth(loginResult?.user));
      toast({
        variant: 'success',
        title: 'Login Successfully ',
      });

      if (window.history.length > 2) {
        router.back();
      } else {
        router.replace('/');
      }
    } catch (e: any) {
      if (e?.shape?.message == 'Your Account is Not Verified') {
        setOtpIsModal(true);
      } else {
        setOtpIsModal(false);
      }

      toast({
        variant: 'destructive',
        title: e.message,
      });
    }
  };
  const today = new Date();

  // Calculate the minimum date (18 years ago from today)
  const minDate = new Date(
    today.getFullYear() - 18,
    today.getMonth(),
    today.getDate(),
  );

  // Format the minimum date as "YYYY-MM-DD" for the input field
  const minDateFormatted = minDate.toISOString().split('T')[0];
  return (
    <section className="body-font pt-24 space-y-24">
      <div className=" mb-24 lg:pb-0 md:pb-0 lg:py-24 md:py-24 mx-auto flex flex-col-reverse px-4 md:px-14 lg:flex-row md:flex-row justify-between gap-10 w-full">
        <div className="hidden w-full h-full md:w-2/3 lg:mb-0 rounded-lg lg:block">
          <SideImage
            image={CarImage}
            text={langContent[lang.lang].Auth.SIDE_CONTENT}
            text2={langContent[lang.lang].Auth.SIDE_SUB_CONTENT}
          />
        </div>
        <Tabs
          defaultValue={defaultValue === 'login' ? 'login' : 'signup'}
          className="flex flex-col flex-wrap  w-full  lg:text-left  rounded-none border-none   bg-card "
        >
          <TabsList className=" w-full rounded-none border-none shadow-none">
            <TabsTrigger
              value="login"
              className="w-full font-black text-md -mt-1 font-sans rounded-none border-none m-0 shadow-none"
            >
              {langContent[lang.lang].Auth.TAB_LOGIN}
            </TabsTrigger>
            <TabsTrigger
              value="signup"
              className="w-full font-sans text-md font-black shadow-none"
            >
              {langContent[lang.lang].Auth.TAB_REGISTER}
            </TabsTrigger>
          </TabsList>
          <TabsContent value="login">
            <Form {...formLogin}>
              <form
                onSubmit={formLogin.handleSubmit(onSubmitLogin)}
                className="justify-center items-center px-2 lg:px-8 pb-4 pt-2 sm:py-4 space-y-4 w-full h-full"
              >
                <div className="w-full">
                  <FormField
                    control={formLogin.control}
                    name="user"
                    render={({ field }) => (
                      <FormItem className="">
                        <FormLabel className="text-xs font-thin text-grayColor">
                          Email address <sup className="">*</sup>
                        </FormLabel>
                        <FormControl>
                          <Input
                            type="text"
                            placeholder="Enter Email Address"
                            {...formLogin.register('user', {
                              onChange(event) {
                                formLogin.setValue(
                                  'user',
                                  event?.target?.value?.trim(),
                                );
                              },
                            })}
                            className="rounded-md"
                          />
                        </FormControl>
                        <div className="relative pb-2 errormsg">
                          <FormMessage />
                        </div>
                      </FormItem>
                    )}
                  />
                  <FormField
                    control={formLogin.control}
                    name="password"
                    render={({ field }) => (
                      <FormItem className="">
                        <FormLabel className="text-xs font-thin text-grayColor">
                          Password <sup className="">*</sup>
                        </FormLabel>
                        <FormControl>
                          <Input
                            type="password"
                            placeholder="Enter Your Password"
                            {...field}
                            className="rounded-md"
                          />
                        </FormControl>
                        <div className="relative pb-2 errormsg">
                          <FormMessage />
                        </div>
                      </FormItem>
                    )}
                  />
                </div>
                <div className="flex flex-col md:flex-row   justify-end items-center gap-4 ">
                  <p
                    className="underline text-xs lg:text-base md:text-base my-auto ltr:self-start rtl:self-end  cursor-pointer"
                    onClick={() => setIsModal(true)}
                  >
                    {langContent[lang.lang].Auth.FORGOT}
                  </p>
                  <Button
                    className=" px-16 lg:w-40 ltr:lg:w-40  rtl:lg:w-56   md:w-40 w-full  text-black font-sans font-[900]   text-xl tracking-[-1px]"
                    variant="clip"
                  >
                    {langContent[lang.lang].Auth.LOGIN_BUTTON}
                  </Button>
                </div>
              </form>
            </Form>
          </TabsContent>

          <TabsContent value="signup">
            <Form {...formSignup}>
              <form
                onSubmit={formSignup.handleSubmit(onSubmitSignup)}
                className=" justify-center items-center px-2 lg:px-8 py-4 space-y-2 w-full h-full"
              >
                <div className="">
                  <div className="flex flex-col sm:flex-row justify-center items-center md:gap-4">
                    <FormField
                      control={formSignup.control}
                      name="first_name"
                      render={({ field }) => (
                        <FormItem className="w-full">
                          <FormLabel className="text-xs font-thin text-grayColor">
                            First Name <sup className="">*</sup>
                          </FormLabel>
                          <FormControl>
                            <Input
                              type="text"
                              placeholder="Enter your first name"
                              {...field}
                              className="rounded-md"
                            />
                          </FormControl>
                          <div className="relative pb-2 errormsg">
                            <FormMessage />
                          </div>
                        </FormItem>
                      )}
                    />
                    <FormField
                      control={formSignup.control}
                      name="last_name"
                      render={({ field }) => (
                        <FormItem className=" w-full">
                          <FormLabel className="text-xs font-thin text-grayColor">
                            Last Name <sup className="">*</sup>
                          </FormLabel>
                          <FormControl>
                            <Input
                              type="text"
                              placeholder="Enter your last name"
                              {...field}
                              className="rounded-md"
                            />
                          </FormControl>
                          <div className="relative pb-2 errormsg">
                            <FormMessage />
                          </div>
                        </FormItem>
                      )}
                    />
                  </div>

                  <div className="flex flex-col sm:flex-row justify-center items-start md:gap-4">
                    <div className="w-full">
                      <FormLabel className="text-xs font-thin text-grayColor">
                        Phone Number <sup className="">*</sup>
                      </FormLabel>
                      <div className="flex flex-row gap-2 mt-2 ">
                        <FormField
                          control={formSignup.control}
                          name="code"
                          render={({ field }) => (
                            <FormItem>
                              <Input
                                type="text"
                                className="rounded-md w-20 "
                                placeholder="+971"
                                maxLength={4}
                                {...field}
                              />
                              <div className="relative pb-2 errormsg">
                                <FormMessage />
                              </div>
                            </FormItem>
                          )}
                        />
                        <FormField
                          control={formSignup.control}
                          name="phone_number"
                          render={({ field }) => (
                            <FormItem className=" w-full">
                              <FormControl className="rounded-md ">
                                <Input
                                  min={0}
                                  type="number"
                                  className="w-full"
                                  placeholder="Enter your phone number"
                                  {...field}
                                />
                              </FormControl>

                              <div className="relative pb-2 errormsg">
                                <FormMessage />
                              </div>
                            </FormItem>
                          )}
                        />
                      </div>
                    </div>

                    <FormField
                      control={formSignup.control}
                      name="gender"
                      render={({ field }) => (
                        <FormItem className="w-full">
                          <FormLabel className="text-xs font-thin text-grayColor">
                            Gender <sup className="">*</sup>
                          </FormLabel>
                          <Select
                            onValueChange={field.onChange}
                            defaultValue={field.value}
                            value={field.value}
                          >
                            <FormControl>
                              <SelectTrigger className=" h-10  bg-inputColor">
                                <SelectValue placeholder={`Select Gender`} />
                              </SelectTrigger>
                            </FormControl>
                            <SelectContent>
                              <SelectGroup>
                                <SelectItem value={'male'}>{'Male'}</SelectItem>
                                <SelectItem value={'female'}>
                                  {'Female'}
                                </SelectItem>
                              </SelectGroup>
                            </SelectContent>
                          </Select>
                          <div className="relative pb-2 errormsg">
                            <FormMessage />
                          </div>
                        </FormItem>
                      )}
                    />
                  </div>

                  <div className="flex flex-col sm:flex-row justify-center items-center md:gap-4">
                    <FormField
                      control={formSignup.control}
                      name="dob"
                      render={({ field }) => (
                        <FormItem className="w-full">
                          <FormLabel className="text-xs font-thin text-grayColor">
                            Date of Birth <sup className="">*</sup>
                          </FormLabel>
                          <FormControl>
                            <Input
                              type={'date'}
                              placeholder={'Enter DOB'}
                              max={minDateFormatted}
                              className="rounded-md "
                              {...formSignup.register('dob', {
                                valueAsDate: true,
                              })}
                            />
                          </FormControl>
                          <div className="relative pb-2 errormsg">
                            <FormMessage />
                          </div>
                        </FormItem>
                      )}
                    />

                    <FormField
                      control={formSignup.control}
                      name="country"
                      render={({ field }) => (
                        <FormItem className="w-full ">
                          <FormLabel className="text-xs text-white">
                            Country/ Region <sup className="text-white">*</sup>
                          </FormLabel>
                          <Select
                            onValueChange={field.onChange}
                            defaultValue={field.value}
                            value={field.value}
                          >
                            <FormControl className="">
                              <SelectTrigger className=" rounded-md h-10 bg-inputColor ">
                                <SelectValue
                                  placeholder="Select your country"
                                  className=""
                                />
                              </SelectTrigger>
                            </FormControl>
                            <SelectContent className="max-h-[300px] overflow-y-auto">
                              <SelectGroup>
                                {countries &&
                                  countries?.map((country, i) => {
                                    return (
                                      <SelectItem key={country} value={country}>
                                        {country?.toUpperCase()}
                                      </SelectItem>
                                    );
                                  })}
                              </SelectGroup>
                            </SelectContent>
                          </Select>

                          <div className="relative pb-2 errormsg">
                            <FormMessage />
                          </div>
                        </FormItem>
                      )}
                    />
                  </div>
                  <FormField
                    control={formSignup.control}
                    name="email"
                    render={({ field }) => (
                      <FormItem className="">
                        <FormLabel className="text-xs  font-thin text-grayColor">
                          Email Address <sup className="">*</sup>
                        </FormLabel>
                        <FormControl>
                          <Input
                            type="text"
                            placeholder="Enter Your Email Address"
                            {...field}
                            className="rounded-md"
                          />
                        </FormControl>
                        <div className="relative pb-2 errormsg">
                          <FormMessage />
                        </div>
                      </FormItem>
                    )}
                  />
                  <FormField
                    control={formSignup.control}
                    name="password"
                    render={({ field }) => (
                      <FormItem className="">
                        <FormLabel className="text-xs font-thin text-grayColor">
                          Password <sup className="">*</sup>
                        </FormLabel>
                        <FormControl>
                          <Input
                            type="password"
                            placeholder="Enter your password"
                            {...field}
                            className="rounded-md"
                          />
                        </FormControl>
                        <div className="relative pb-2 errormsg">
                          <FormMessage />
                        </div>
                      </FormItem>
                    )}
                  />
                </div>
                <div className="mt-16 flex flex-col lg:flex-row md:flex-row justify-between items-center gap-6 ">
                  <p className="text-lightColor text-gray-400 font-extralight text-xs w-full lg:w-96  md:w-96  ltr:text-left rtl:text-right">
                    {langContent[lang.lang].Auth.REGISTER_INFO}{' '}
                    <span className="text-white underline ">
                      {' '}
                      <Link href="/privacy-policy ">
                        {' '}
                        {langContent[lang.lang].Auth.REGISTER_SUB_INFO}{' '}
                      </Link>
                    </span>
                  </p>
                  <Button
                    className="  lg:w-52 md:w-52 w-full     text-black font-sans font-[900]   text-xl tracking-[-1px]"
                    variant="clip"
                  >
                    {langContent[lang.lang].Auth.REGISTER_BUTTON}
                  </Button>
                </div>
              </form>
            </Form>
          </TabsContent>
        </Tabs>
      </div>
      <LoadingDialog
        open={registerCustomer.isLoading || loginCustomer.isLoading}
        text={'Loading...'}
      />

      <ForgotPasswordDailog isModal={isModal} setIsModal={setIsModal} />
      <OtpVerificationDailog
        otpIsModal={otpIsModal}
        setOtpIsModal={setOtpIsModal}
        emailOrUser={formLogin.getValues('user') || (user ?? '')}
      />
    </section>
  );
}
const countryCode = [
  {
    code: '+971',
  },
];
