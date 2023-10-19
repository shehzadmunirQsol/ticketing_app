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

import ReCAPTCHA from "react-google-recaptcha"
import Image  from 'next/image';

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
  console.log({ recaptchaToken })
  // Handle Contact us
  const contactUs = trpc.contact.contact.useMutation({
    onSuccess: async (res: any) => {
      setRecapthaToken(false);

      console.log(res, 'res');
      // props.setIsModal(false);

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


  const validate = (value: string) => {
    const matches = value.match(
      /^[0-9]/
    );
    return matches && matches?.length > 0 || "Please enter a valid number";
  };

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
      const contact = await contactUs.mutateAsync(values);
      if (contact) {
        toast({
          variant: 'success',
          title: 'Email Sent.',
        });
      }
    } catch (error) {
      console.log(error)
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
        <div className="pb-6 flex flex-col h-full lg:max-h-[700px] w-full    lg:w-3/5  mx-auto lg:mx-0  lg:text-left  rounded-none border-none  bg-card ">
          <div className="font-black  py-4 ">
            <p className="text-xl pl-6 px-4 lg:px-8">Contact Us</p>
            <hr className=" opacity-20 mt-4" />

            <div className="flex flex-col justify-start align-start pl-6 px-4 lg:px-8 gap-2 mt-4">
              <Image
                className="object-contain mb-2"
                src={location}
                quality={100}
                alt="Location"
              />
              <div className="text-grayColor text-lg font-[600]">Address</div>
              <div className="flex flex-col text-grayColor font-thin">
                <span>Dream Car Giveaways Limited</span>
                <span>Berry Hill Industrial Estate,</span>
                <span>Droitwich, WR9 9AB</span>
              </div>
            </div>
            <div className="flex flex-col justify-start align-start pl-6 px-4 lg:px-8 gap-2 mt-4">
              <Image
                className="object-contain mb-2"
                src={phone}
                quality={100}
                alt="Location"
              />
              <div className="text-grayColor text-lg font-[600]">General enquiry</div>
              <div className="flex flex-col text-grayColor font-thin">
                <span className="mb-2">01386719064</span>
                <span>Info@dreamcargiveaways.co.uk</span>
              </div>
            </div>
            <div className="flex flex-col justify-start align-start pl-6 px-4 lg:px-8 gap-2 mt-4">
              <Image
                className="object-contain mb-2"
                src={mail}
                quality={100}
                alt="Location"
              />
              <div className="text-grayColor text-lg font-[600]">Sell your car</div>
              <div className="text-grayColor font-thin">
                sellyourcar@dreamcargiveaways.co.uk
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
                      rules={{ validate }}
                      render={({ field }) => (
                        <FormItem className=" w-full">
                          <FormControl className="rounded-md bg-inputColor">
                            <Input
                              type="text"
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
              <div className="flex flex-col sm:flex-row justify-between items-center gap-6 h-18">
                <div className="h-fit object-contain w-fit ">
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