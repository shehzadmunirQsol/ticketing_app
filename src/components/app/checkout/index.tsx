'use client';
import React, { useEffect, useState } from 'react';
import { Button } from '@/ui/button';
import {
  Form,
  FormControl,
  FormField,
  FormItem,
  FormLabel,
  FormMessage,
} from '@/ui/form';
import {
  Select,
  SelectItem,
  SelectTrigger,
  SelectContent,
  SelectGroup,
  SelectValue,
} from '@/ui/select';
import { Input } from '@/ui/input';
import { useForm } from 'react-hook-form';
import Group17 from '~/public/assets/icons/Group17.png';
import Image from 'next/image';
import Glow from '~/components/common/glow';
import { zodResolver } from '@hookform/resolvers/zod';
import { CouponModal } from './Coupon';
import { CheckoutDialog } from '~/components/common/modal/checkout';

const Checkout = () => {
  // Handle Coupon Dailog
  const [isModal, setIsModal] = React.useState(false);
  const [selectedItem, setSelectedItem] = React.useState({});
  const [title, setTitle] = React.useState('Enter Card Detail');
  const [type, setType] = React.useState('');
  const [isCardModal, setIsCardModal] = React.useState(false);

  // 1. Define your form.
  const form = useForm<any>({
    // resolver: zodResolver(createCheckoutSchema),
  });

  const onSubmitCheckout = async (values: any) => {
    setIsCardModal(true);
    console.log(values, 'loginResult');
  };

  const data = [
    {
      country: 'United Arab Emirates',
    },
  ];
  const States = [
    {
      state: 'United Arab Emirates',
    },
  ];

  const product = [
    {
      name: 'Win This 800BHP Ferrari E63s Night Edition + AED 1,000 Cash!',
      price: 120,
    },
    {
      name: 'Win This 800BHP Ferrari E63s Night Edition + AED 1,000 Cash!',
      price: 120,
    },
    {
      name: 'Win This 800BHP Ferrari E63s Night Edition + AED 1,000 Cash!',
      price: 120,
    },
    {
      name: 'Win This 800BHP Ferrari E63s Night Edition + AED 1,000 Cash!',
      price: 120,
    },
  ];

  const countryCode = [
    {
      code: '+971',
    },
  ];

  return (
    <div className="my-40 px-4 md:px-14 lg:px-16">
      <Form {...form}>
        <form
          onSubmit={form.handleSubmit(onSubmitCheckout)}
          className="justify-center items-center  py-4 space-y-4"
        >
          <div>
            <p className="lg:text-5xl md:text-4xl text-2xl font-black uppercase mb-10">
              Checkout
            </p>
          </div>
          <div className="flex flex-col gap-8 lg:flex-row md:flex-row justify-between w-full ">
            <div className="flex-1">
              <p className="text-xl font-black">Billing Details</p>
              <div>
                <div className="flex flex-col lg:flex-row md:flex-row gap-2  w-full justify-between">
                  <FormField
                    control={form.control}
                    name="firstname"
                    render={({ field }) => (
                      <FormItem className=" w-full mb-6">
                        <FormLabel className="text-xs font-thin text-grayColor">
                          Name
                        </FormLabel>
                        <FormControl className="rounded-md bg-inputColor">
                          <Input
                            type="text"
                            placeholder="Enter your name"
                            {...field}
                          />
                        </FormControl>
                        <FormMessage />
                      </FormItem>
                    )}
                  />
                  <FormField
                    control={form.control}
                    name="lastname"
                    render={({ field }) => (
                      <FormItem className=" w-full mb-6">
                        <FormLabel className="text-xs font-thin text-grayColor">
                          Last Name
                        </FormLabel>
                        <FormControl className="rounded-md bg-inputColor">
                          <Input
                            type="text"
                            placeholder="Enter last name"
                            {...field}
                          />
                        </FormControl>
                        <FormMessage />
                      </FormItem>
                    )}
                  />
                </div>
                <FormField
                  control={form.control}
                  name="street"
                  render={({ field }) => (
                    <FormItem className="w-full mb-6">
                      <FormLabel className="text-xs font-thin text-grayColor">
                        Street Address
                      </FormLabel>
                      <FormControl className="rounded-md bg-inputColor">
                        <Input
                          type="text"
                          placeholder="House number and street name"
                          {...field}
                        />
                      </FormControl>
                      <FormMessage />
                    </FormItem>
                  )}
                />
                <FormField
                  control={form.control}
                  name="apartment"
                  render={({ field }) => (
                    <FormItem className="w-full mb-6">
                      <FormControl className="rounded-md bg-inputColor">
                        <Input
                          type="text"
                          placeholder="Apartment, suit, unit etc. (Optional) "
                          {...field}
                        />
                      </FormControl>
                      <FormMessage />
                    </FormItem>
                  )}
                />
                <div className="flex flex-col lg:flex-row md:flex-row gap-2  w-full justify-between">
                  <div className="w-full mb-6">
                    <FormField
                      control={form.control}
                      name="country"
                      render={({ field }) => (
                        <FormItem>
                          <FormLabel className="text-xs font-thin text-grayColor">
                            Country/ Region
                          </FormLabel>
                          <Select
                            onValueChange={field.onChange}
                            defaultValue={field.value}
                            value={field.value}
                          >
                            <FormControl>
                              <SelectTrigger className=" rounded-none  ">
                                <SelectValue placeholder="Select your country" />
                              </SelectTrigger>
                            </FormControl>
                            <SelectContent>
                              <SelectGroup>
                                {data?.map((item, i) => (
                                  <SelectItem key={i} value={item.country}>
                                    {item?.country?.toUpperCase()}
                                  </SelectItem>
                                ))}
                              </SelectGroup>
                            </SelectContent>
                          </Select>

                          <FormMessage />
                        </FormItem>
                      )}
                    />
                  </div>
                  <div className="w-full mb-6">
                    <FormField
                      control={form.control}
                      name="state"
                      render={({ field }) => (
                        <FormItem>
                          <FormLabel className="text-xs font-thin text-grayColor">
                            State
                          </FormLabel>
                          <Select
                            onValueChange={field.onChange}
                            defaultValue={field.value}
                            value={field.value}
                          >
                            <FormControl>
                              <SelectTrigger className=" rounded-none  ">
                                <SelectValue placeholder="Select your state" />
                              </SelectTrigger>
                            </FormControl>
                            <SelectContent>
                              <SelectGroup>
                                {States?.map((item, i) => (
                                  <SelectItem key={i} value={item.state}>
                                    {item?.state?.toUpperCase()}
                                  </SelectItem>
                                ))}
                              </SelectGroup>
                            </SelectContent>
                          </Select>

                          <FormMessage />
                        </FormItem>
                      )}
                    />
                  </div>
                </div>
                <div className="flex flex-col lg:flex-row md:flex-row gap-2  w-full justify-between">
                  <FormField
                    control={form.control}
                    name="city"
                    render={({ field }) => (
                      <FormItem className=" w-full mb-6">
                        <FormLabel className="text-xs font-thin text-grayColor">
                          Town/City
                        </FormLabel>
                        <FormControl className="rounded-md bg-inputColor">
                          <Input
                            type="text"
                            placeholder="Enter the city"
                            {...field}
                          />
                        </FormControl>
                        <FormMessage />
                      </FormItem>
                    )}
                  />
                  <FormField
                    control={form.control}
                    name="postcode"
                    render={({ field }) => (
                      <FormItem className=" w-full mb-6">
                        <FormLabel className="text-xs font-thin text-grayColor">
                          Postcode
                        </FormLabel>
                        <FormControl className="rounded-md bg-inputColor">
                          <Input
                            type="text"
                            placeholder="Enter your postcode"
                            {...field}
                          />
                        </FormControl>
                        <FormMessage />
                      </FormItem>
                    )}
                  />
                </div>
                <FormField
                  control={form.control}
                  name="email"
                  render={({ field }) => (
                    <FormItem className="mb-6 w-full">
                      <FormLabel className="text-xs font-thin text-grayColor">
                        Email
                      </FormLabel>
                      <FormControl className="rounded-md bg-inputColor">
                        <Input
                          type="text"
                          placeholder="Enter your email address"
                          {...field}
                        />
                      </FormControl>
                      <FormMessage />
                    </FormItem>
                  )}
                />

                <div className="flex flex-col items-center   md:flex-row gap-2  w-full justify-between">
                  <div className=" w-full ">
                    <p className="text-xs font-thin text-grayColor  mb-3 ">
                      Phone Number
                    </p>
                    <div className="flex flex-row gap-2 ">
                      <FormField
                        control={form.control}
                        name="code"
                        render={({ field }) => (
                          <FormItem>
                            <Select
                              onValueChange={field.onChange}
                              defaultValue={field.value}
                              value={field.value}
                            >
                              <FormControl>
                                <SelectTrigger className=" rounded-none  ">
                                  <SelectValue placeholder="+971" />
                                </SelectTrigger>
                              </FormControl>
                              <SelectContent>
                                <SelectGroup>
                                  {countryCode?.map((item, i) => (
                                    <SelectItem key={i} value={item.code}>
                                      {item?.code}
                                    </SelectItem>
                                  ))}
                                </SelectGroup>
                              </SelectContent>
                            </Select>

                            <FormMessage />
                          </FormItem>
                        )}
                      />
                      <FormField
                        control={form.control}
                        name="number"
                        render={({ field }) => (
                          <FormItem className=" w-full">
                            {/* <FormLabel className="text-xs font-thin text-grayColor">
                            Email
                          </FormLabel> */}
                            <FormControl className="rounded-md bg-inputColor">
                              <Input
                                type="text"
                                placeholder="Enter your phone number"
                                {...field}
                              />
                            </FormControl>
                            <FormMessage />
                          </FormItem>
                        )}
                      />
                    </div>
                  </div>
                  <div className="flex-1 w-full ">
                    <FormField
                      control={form.control}
                      name="dateofbirth"
                      render={({ field }) => (
                        <FormItem className="mb-2 w-full">
                          <FormLabel className="text-xs font-thin text-grayColor">
                            Date of Birth
                          </FormLabel>
                          <FormControl className="rounded-md bg-inputColor">
                            <Input
                              type="date"
                              placeholder="Enter your email address"
                              {...field}
                            />
                          </FormControl>
                          <FormMessage />
                        </FormItem>
                      )}
                    />
                  </div>
                </div>
              </div>
            </div>
            <div className="border-r border-lightColorBorder  mx-4"></div>
            <div className="flex-1 ">
              <div className="flex flex-row justify-between items-center mb-6">
                <p className="lg:text-2xl md:lg:text-xlfont-bold">
                  Order Summary
                </p>
                <p
                  className="text-sm lg:text-base cursor-pointer"
                  onClick={() => setIsModal(true)}
                >
                  Have a coupon code?
                </p>
              </div>

              <div className=" h-[300px] overflow-x-auto">
                {product.map((item, i) => {
                  return (
                    <div
                      className="flex flex-row justify-between mb-10 "
                      key={i}
                    >
                      <p className="lg:text-2xl md:lg:text-xl   w-[60%]">
                        {item.name}
                      </p>
                      <p className="font-black text-lg lg:text-xl ">{`AED ${item.price.toFixed(
                        2,
                      )}`}</p>
                    </div>
                  );
                })}
              </div>

              <div className="flex flex-row justify-between mb-10">
                <p className="lg:text-2xl md:lg:text-xl font-black">Total:</p>
                <p className="font-black text-lg lg:text-xl text-primary">
                  AED 120.00
                </p>
              </div>
              <p className="text-lightTextColor mb-6 lg:text-base md:text-sm text-xs">
                Your personal data will be used to process your order, support
                your experience throughout this website, and for other purposes
                described in our{' '}
                <span className="text-white"> privacy policy</span>.
              </p>
              <div className="flex flex-row gap-2 justify-start w-full  lg:w-[60%] md:w-[60%] items-center mb-20">
                <div>
                  <Input type="checkbox" className="accent-white text-2xl " />
                </div>
                <p className="text-lightTextColor text-sm">
                  I’m 18 years old or over and i have read and agree to the
                  website
                  <span className="text-white"> Terms & Conditions</span>.
                </p>
              </div>
              <div className="flex flex-row gap-4 justify-center ">
                <p className="text-sm">We accept</p>
                <Image
                  className="w-64 object-contain  "
                  src={Group17}
                  quality={100}
                  alt="Sunset in the mountains"
                />
              </div>
              <Glow className=" absolute bottom-[750px]  lg:bottom-[440px] md:bottom-[440px] -right-16  w-1/5 h-[350px] -z-2 opacity-50 " />

              <Button
                className=" px-16 mt-10 w-full  text-black font-sans font-[900]   text-xl tracking-[-1px]"
                variant="clip"
              >
                PAY WITH CARD
              </Button>
            </div>
          </div>
        </form>
      </Form>
      <CheckoutDialog
        selectedItem={selectedItem}
        setSelectedItem={setSelectedItem}
        title={title}
        setTitle={setTitle}
        isModal={isCardModal}
        setIsModal={setIsCardModal}
        type={type}
        setType={setType}
      />
      <CouponModal isModal={isModal} setIsModal={setIsModal} />
    </div>
  );
};

export default Checkout;
