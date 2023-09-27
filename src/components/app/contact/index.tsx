import React, { createRef, useState } from 'react';
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
import { trpc } from '~/utils/trpc';
import { getS3ImageUrl } from '~/service/api/s3Url.service';
import SideImage from '../../common/SideImage';
import { contactSchema, contactSchemaInput } from '~/schema/contact';
import { useToast } from '~/components/ui/use-toast';
import Link from 'next/link';
import ContactImage from '../../../public/assets/contact-us.svg';
import { zodResolver } from '@hookform/resolvers/zod';

import ReCAPTCHA from "react-google-recaptcha"

export default function Contact() {
  const { toast } = useToast();
  const router = useRouter();
  const recaptchaRef: any = createRef();
  const [recaptchaToken, setRecapthaToken] = useState<string>('');

  // 1. Define your form.
  const form = useForm<contactSchemaInput>({
    resolver: zodResolver(contactSchema),
  });


  const showResponse = (response: any) => {
    console.log({ response });
    if (response) {
      setRecapthaToken(() => recaptchaRef.current.getValue());
    }

    //call to a backend to verify against recaptcha with private key
  };
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
      form.setValue('code', '+971');
      form.setValue('number', '');
      form.setValue('message', '');
      recaptchaRef.current.reset();
      setRecapthaToken("");
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
    if (!recaptchaToken) {
      toast({
        variant: 'destructive',
        title: 'Invalid Captcha: Please try again.',
      });
      return;
    }

    try {
      await contactUs.mutateAsync(values);
    } catch (error) {
      console.log(error)
    }
  };

  return (
    <section className="body-font pt-24 space-y-24">
      <div className="px-4 my-10 md:px-14 md:my-24 flex gap-14 w-full">
        <div className="w-2/5 mb-5 lg:mb-0 rounded-lg hidden lg:block">
          <SideImage
            image={ContactImage}
            text={'Connect with Us for '}
            text2={'Support, Questions'}
          />
        </div>
        <div className="w-96 pb-6 flex flex-col flex-wrap   lg:w-3/5 md:w-full  lg:text-left  rounded-none border-none  bg-card">
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

                      <div className="relative pb-2">
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

                      <div className="relative pb-2">
                        <FormMessage />
                      </div>
                    </FormItem>
                  )}
                />

                <div className=" w-full ">
                  <p className="text-xs font-thin text-grayColor  mb-2 ">
                    Phone Number*
                  </p>
                  <div className="flex items-center flex-row gap-2 ">
                    <FormField
                      control={form.control}
                      name="code"
                      defaultValue="+971"
                      render={({ field }) => (
                        <FormItem>
                          <Select
                            onValueChange={field.onChange}
                            defaultValue={field.value}
                            value={field.value}
                          >
                            <FormControl className="rounded-md bg-inputColor">
                              <SelectTrigger
                                defaultValue={'+971'}
                                className=" rounded-md  "
                              >
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

                          <div className="relative pb-2">
                            <FormMessage />
                          </div>
                        </FormItem>
                      )}
                    />
                    <FormField
                      control={form.control}
                      name="number"
                      render={({ field }) => (
                        <FormItem className=" w-full">
                          <FormControl className="rounded-md bg-inputColor">
                            <Input
                              type="number"
                              maxLength={9}
                              placeholder="Enter your phone number"
                              {...field}
                            />
                          </FormControl>

                          <div className="relative pb-2">
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

                      <div className="relative pb-2">
                        <FormMessage />
                      </div>
                    </FormItem>
                  )}
                />
              </div>
              <div className="flex flex-col lg:flex-row md:flex-row justify-between items-center gap-6 ">
                <ReCAPTCHA
                  ref={recaptchaRef}
                  size="normal"
                  badge="inline"
                  sitekey={process.env.NEXT_PUBLIC_SITE_KEY as string}
                  onChange={showResponse}
                />

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
