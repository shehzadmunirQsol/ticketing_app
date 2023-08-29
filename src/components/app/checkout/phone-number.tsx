'use client';
import React, { useEffect, useState } from 'react';
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
import { useRouter } from 'next/router';
import { trpc } from '~/utils/trpc';

const PhoneNumber = (props:any) => {
  const handleCheckout = useForm<any>();

  const onSubmitCheckout = async (values: any) => {
    // const loginResult = await loginCustomer.mutateAsync(values);
    console.log(values, 'loginResult');
  };

  //   handle select
  function onValueChange(value: string) {
    console.log(value, 'value');
    // const language = data?.data.find((lang) => lang.code === value);

    // props.languageHandler({
    //   id: language?.id as number,
    //   code: language?.code as 'en' | 'ar',
    // });
  }

  const countryCode = [
    {
      code: '+971',
    },
  ];

  return (
    <div>
      <div>
        <FormLabel className="text-xs font-thin text-grayColor pb-3">
          Phone Number
        </FormLabel>
        <div className="flex flex-col lg:flex-row md:flex-row gap-2  w-full justify-between">
          <div>
            <Select onValueChange={props?.onValueChange}>
              <SelectTrigger className="bg-background h-10 full">
                <SelectValue placeholder="+971" />
              </SelectTrigger>
              <SelectContent>
                <SelectGroup>
                  {countryCode?.map((item, i) => (
                    <SelectItem key={i} value={item.code}>
                      {item?.code?.toUpperCase()}
                    </SelectItem>
                  ))}
                </SelectGroup>
              </SelectContent>
            </Select>
          </div>
          <FormField
            control={handleCheckout.control}
            name="number"
            render={({ field }) => (
              <FormItem className=" w-full flex">
                <FormControl className="rounded-md flex-1 bg-inputColor">
                  <Input
                    type="text"
                    placeholder="Enter phone number"
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
  );
};

export default PhoneNumber;
