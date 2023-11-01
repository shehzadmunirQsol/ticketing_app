import React, { createRef, useRef, useState } from 'react';
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
import location from '../../../public/assets/icons/location.svg';
import phone from '../../../public/assets/icons/phone.svg';
import mail from '../../../public/assets/icons/mail.svg';
import { zodResolver } from '@hookform/resolvers/zod';

import ReCAPTCHA from 'react-google-recaptcha';
import Image from 'next/image';

export default function Contact() {
  const { toast } = useToast();
  const router = useRouter();
  const recaptchaRef: any = useRef({});
  const [recaptchaToken, setRecapthaToken] = useState<any>();

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
  console.log({ recaptchaToken });
  // Handle Contact us
  const contactUs = trpc.contact.contact.useMutation({
    onSuccess: async (res: any) => {
      setRecapthaToken(false);

      console.log(res, 'res');
      // props.setIsModal(false);

      form.setValue('name', '');
      form.setValue('email', '');
      form.setValue('code', '');
      form.setValue('number', '');
      form.setValue('message', '');
      recaptchaRef.current.reset();
      setRecapthaToken('');
    },
    onError: (err) => {
      console.log(err.message, 'err');
    },
  });

  const validate = (value: string) => {
    const matches = value.match(/^[0-9]/);
    return (matches && matches?.length > 0) || 'Please enter a valid number';
  };

  // Contact
  const onSubmitContact = async (values: any) => {
    if (!recaptchaToken) {
      toast({
        variant: 'destructive',
        title: 'Please verify captcha!',
      });
      return;
    }

    try {
      const contact = await contactUs.mutateAsync(values);
      if (contact) {
        toast({
          variant: 'success',
          title: 'Email Sent successfully!',
        });
      }
    } catch (error) {
      console.log(error);
    }
  };

  return (
    <section className="body-font pt-24 space-y-24">
      <div className="px-4 my-10 md:px-14 md:my-24 flex gap-14 w-full ">
        <div className="w-2/5 mb-5 lg:mb-0 rounded-lg hidden lg:block  ">
          <SideImage
            image={ContactImage}
            text={'Connect with Us for '}
            text2={'Support, Questions'}
          />
        </div>
        <div className="pb-6 flex flex-col h-full  w-full    lg:w-3/5  mx-auto lg:mx-0  lg:text-left  rounded-none border-none  bg-card ">
          <div className="font-black  pt-4 pb-6">
            <p className="text-xl pl-6 px-4 lg:px-8">Contact Us</p>
            <hr className=" opacity-20 mt-4" />

            <div className="flex flex-col justify-start align-start pl-6 px-4 lg:px-8 gap-2 py-3">
              <div className="flex gap-3 items-center">
                <Image
                  className="object-contain"
                  src={location}
                  quality={100}
                  alt="Location"
                />
                <div className="text-grayColor text-lg font-[600]">Address</div>
              </div>
              <div className="flex flex-col text-grayColor font-thin">
                <span>Winnar LLC</span>
                <span>Office 16, 17th Floor,</span>
                <span>Centrum Business Lounge</span>
                <span>Bay Gate Tower, Business Bay,</span>
                <span>Dubai, UAE</span>
              </div>
            </div>
            <div className="flex flex-col justify-start align-start pl-6 px-4 lg:px-8 gap-2 py-3">
              <div className="flex gap-3 items-center">
                <Image
                  className="object-contain"
                  src={phone}
                  quality={100}
                  alt="Location"
                />
                <div className="text-grayColor text-lg font-[600]">
                  General enquiry
                </div>
              </div>
              <div className="flex flex-col text-grayColor font-thin">
                <span className="mb-2">01386719064</span>
                <span>contact@winnar.com</span>
              </div>
            </div>
            <div className="flex flex-col justify-start align-start pl-6 px-4 lg:px-8 gap-2 py-3">
              <div className="flex gap-3 items-center">
                <Image
                  className="object-contain"
                  src={mail}
                  quality={100}
                  alt="Location"
                />
                <div className="text-grayColor text-lg font-[600]">
                  Sell your car
                </div>
              </div>
              <div className="text-grayColor font-thin">
                sellyourcar@winnar.com
              </div>
            </div>
          </div>
          <Form {...form}>
            <form
              onSubmit={form.handleSubmit(onSubmitContact)}
              className="justify-center items-center px-4 lg:px-8   space-y-4"
            >
              <div className="w-full">
                <FormField
                  control={form.control}
                  name="name"
                  render={({ field }) => (
                    <FormItem className="">
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
                    <FormItem className="">
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

                <div className="space-y-2 w-full">
                  <FormLabel className="text-xs  font-thin text-grayColor">
                    Phone Number*
                  </FormLabel>
                  <div className="flex items-start  gap-2 ">
                    <FormField
                      control={form.control}
                      name="code"
                      render={({ field }) => (
                        <FormItem>
                          <Input
                            type="text"
                            className="rounded-md w-20 bg-inputColor"
                            placeholder="+971"
                            maxLength={4}
                            {...field}
                          />
                          <div className="relative">
                            <FormMessage />
                          </div>
                        </FormItem>
                      )}
                    />
                    <FormField
                      control={form.control}
                      name="number"
                      rules={{ validate }}
                      render={({ field }) => (
                        <FormItem className=" w-full">
                          <FormControl className="rounded-md bg-inputColor">
                            <Input
                              type="text"
                              maxLength={15}
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
              <div className="flex flex-col sm:flex-row justify-between items-center gap-6 h-18">
                <div className="h-fit object-contain w-fit capchabx">
                  <ReCAPTCHA
                    ref={recaptchaRef}
                    size="normal"
                    badge="inline"
                    theme="dark"
                    sitekey={process.env.NEXT_PUBLIC_SITE_KEY as string}
                    onChange={showResponse}
                  />
                </div>

                <Button
                  className="  lg:w-52 md:w-52 w-full     text-black font-sans font-[900]   text-xl tracking-[-1px]"
                  variant="clip"
                  disabled={contactUs.isLoading}
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
