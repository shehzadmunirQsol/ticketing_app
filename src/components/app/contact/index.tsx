import React from 'react';
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
import ReCAPTCHA from 'react-google-recaptcha';

import { Textarea } from '~/components/ui/textarea';
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
import { contactSchema, contactSchemaInput } from '~/schema/contact';
import { useToast } from '~/components/ui/use-toast';
import Link from 'next/link';
// import { ForgotPasswordDailog } from './ForgotPassword';
import ContactImage from '../../../public/assets/contact-us.svg';
import { zodResolver } from '@hookform/resolvers/zod';

export default function Contact() {
  const { toast } = useToast();
  const router = useRouter();

  // 1. Define your form.
  const form = useForm<contactSchemaInput>({
    resolver: zodResolver(contactSchema),
  });

  // Handle Contact us
  const contactUs = trpc.contact.contact.useMutation({
    onSuccess: async (res: any) => {
      console.log(res, 'res');
      // props.setIsModal(false);
      toast({
        variant: 'success',
        title: 'Email Sent.',
      });
      form.setValue('name', '');
      form.setValue('email', '');
      form.setValue('code', '');
      form.setValue('number', '');
      form.setValue('message', '');
    },
    onError: (err) => {
      console.log(err.message, 'err');
    },
  });

  const countryCode = [
    {
      code: '+971',
    },
  ];

  // Contact
  const onSubmitContact = async (values: any) => {
    console.log(values, 'Working');
    const resp = await contactUs.mutateAsync(values);
    console.log(resp, 'customer resp');
  };

  return (
    <section className="body-font  ">
      <div className="px-5 mb-10 sm:mb-20 pt-16 pb-10 lg:pb-0 md:pb-0 lg:py-24 md:py-24 mx-auto flex flex-col-reverse lg:flex-row md:flex-row gap-8 mt-6 max-w-[1300px]">
        <div className="lg:w-2/5 w-full h-full mb-5 lg:mb-0 rounded-lg hidden  lg:block  ">
          <SideImage
            image={ContactImage}
            text={'Connect with Us for '}
            text2={'Support, Questions'}
          />
        </div>
        <div className="flex flex-col flex-wrap   lg:w-3/5 md:w-full  lg:text-left  rounded-none border-none  lg:mr-6 bg-card">
          <div className="font-black  py-4 ">
            <p className="text-xl pl-6 px-4 lg:px-8">Contact Us</p>
            <hr className=" opacity-20 mt-4" />
          </div>
          <Form {...form}>
            <form
              onSubmit={form.handleSubmit(onSubmitContact)}
              className="justify-center items-center px-4 lg:px-8  space-y-4"
            >
              <div className="w-full">
                <FormField
                  control={form.control}
                  name="name"

                  render={({ field }) => (
                    <FormItem className="mb-4 ">
                      <FormLabel className="text-xs font-thin  text-grayColor">
                        Your Name*
                      </FormLabel>
                      <FormControl className="rounded-md bg-inputColor">
                        <Input
                          type="text"
                          placeholder="Enter Your name "
                          {...field}
                        />
                      </FormControl>

                      <div className='relative pb-2'>
                        <FormMessage />
                      </div>
                    </FormItem>
                  )}
                />
                <FormField
                  control={form.control}
                  name="email"
                  render={({ field }) => (
                    <FormItem className="mb-6 ">
                      <FormLabel className="text-xs  font-thin text-grayColor">
                        Email Address*
                      </FormLabel>
                      <FormControl className="rounded-md bg-inputColor mb-6">
                        <Input
                          type="text"
                          placeholder="Enter your email address"
                          {...field}
                        />
                      </FormControl>

                      <div className='relative pb-2'>
                        <FormMessage />
                      </div>
                    </FormItem>
                  )}
                />

                <div className=" w-full ">
                  <p className="text-xs font-thin text-grayColor  mb-2 ">
                    Phone Number
                  </p>
                  <div className="flex flex-row gap-2 ">
                    <FormField
                      control={form.control}
                      name="code"
                      defaultValue='+971'
                      render={({ field }) => (
                        <FormItem>
                          <Select
                            onValueChange={field.onChange}
                            defaultValue={field.value}
                            value={field.value}
                          >
                            <FormControl className="rounded-md bg-inputColor">
                              <SelectTrigger defaultValue={"+971"} className=" rounded-md  ">
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


                          <div className='relative pb-2'>
                            <FormMessage />
                          </div>
                        </FormItem>
                      )}
                    />
                    <FormField
                      control={form.control}
                      name="number"
                      render={({ field }) => (
                        <FormItem className=" w-full mb-4">
                          <FormControl className="rounded-md bg-inputColor">
                            <Input
                              type="number"
                              max={9}
                              placeholder="Enter your phone number"
                              {...field}

                            />
                          </FormControl>

                          <div className='relative pb-2'>
                            <FormMessage />
                          </div>
                        </FormItem>
                      )}
                    />
                  </div>
                </div>

                <FormField
                  control={form.control}
                  name="message"
                  render={({ field }) => (
                    <FormItem className="mb-4 ">
                      <FormLabel className="text-xs  font-thin text-grayColor">
                        Message*
                      </FormLabel>
                      <FormControl className="rounded-md bg-inputColor">
                        <Textarea
                          placeholder="Type your message here..."
                          {...field}
                        />
                      </FormControl>

                      <div className='relative pb-2'>
                        <FormMessage />
                      </div>
                    </FormItem>
                  )}
                />
              </div>
              <div className="flex flex-col lg:flex-row md:flex-row justify-between items-center gap-6 ">

                {/* <div className='overflow-hidden max-w-[295px] max-h-[70px] rounded-md'>
                  <ReCAPTCHA
                    // ref={recaptchaRef}
                    size="normal"
                    badge="inline"
                    theme="dark"
                    sitekey={process.env.NEXT_PUBLIC_SITE_KEY}
                  // onChange={showResponse}
                  />
                </div> */}
                <Button
                  className="  lg:w-52 md:w-52 w-full     text-black font-sans font-[900]   text-xl tracking-[-1px]"
                  variant="clip"
                >
                  SEND MESSAGE
                </Button>
              </div>
            </form>
          </Form>
        </div>
      </div>
    </section>
  );
}

// export default LoginSignup;
