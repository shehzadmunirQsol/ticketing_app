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
import {
  Select,
  SelectItem,
  SelectTrigger,
  SelectContent,
  SelectGroup,
  SelectValue,
} from '@/ui/select';
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
      // form.setValue('name', '');
      // form.setValue('email', '');
      // form.setValue('code', '');
      // form.setValue('number', '');
      // form.setValue('message', '');
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
      <div className="px-5 pt-16 pb-10 lg:pb-0 md:pb-0 lg:py-24 md:py-24 mx-auto flex flex-col-reverse lg:flex-row md:flex-row gap-14 mt-6 ">
        <div className="lg:w-2/3 md:w-2/3 w-full h-full mb-5 lg:mb-0 rounded-lg hidden  lg:block  ">
          <SideImage
            image={ContactImage}
            text={'Connect with Us for '}
            text2={'Support, Questions'}
          />
        </div>
        <div className="flex flex-col flex-wrap   lg:w-2/2 md:w-full  lg:text-left  rounded-none border-none  lg:mr-6 bg-card">
          <div className="font-black  py-4 ">
            <p className="text-xl pl-6">Contact Us</p>
            <hr className=" opacity-20 mt-4" />
          </div>
          <Form {...form}>
            <form
              onSubmit={form.handleSubmit(onSubmitContact)}
              className="justify-center items-center px-2 lg:px-8 py-4 space-y-4"
            >
              <div className="w-full">
                <FormField
                  control={form.control}
                  name="name"

                  render={({ field }) => (
                    <FormItem className="mb-6 lg:mb-10 md:mb-10">
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
                      <FormMessage />
                    </FormItem>
                  )}
                />
                <FormField
                  control={form.control}
                  name="email"
                  render={({ field }) => (
                    <FormItem className="mb-6 lg:mb-10 md:mb-10">
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
                      <FormMessage />
                    </FormItem>
                  )}
                />

                <div className=" w-full ">
                  <p className="text-xs font-thin text-grayColor  mb-3 ">
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
                              <SelectTrigger defaultValue={"+971"}  className=" rounded-none  ">
                                <SelectValue  placeholder="+971" />
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
                        <FormItem className=" w-full mb-8">
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

                <FormField
                  control={form.control}
                  name="message"
                  render={({ field }) => (
                    <FormItem className="mb-6 lg:mb-10 md:mb-10">
                      <FormLabel className="text-xs  font-thin text-grayColor">
                        Message*
                      </FormLabel>
                      <FormControl className="rounded-md bg-inputColor">
                        <Textarea
                          placeholder="Type your message here..."
                          {...field}
                        />
                      </FormControl>
                      <FormMessage />
                    </FormItem>
                  )}
                />
              </div>
              <div className="flex flex-col lg:flex-row md:flex-row justify-between items-center gap-6 mt-56">
                <p className="text-lightColor font-extralight text-xs w-full lg:w-96  md:w-96">
                  Your personal data will be used to process your order, support
                  your experience throughout this website, and for other
                  purposes described in our privacy policy.
                </p>
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
