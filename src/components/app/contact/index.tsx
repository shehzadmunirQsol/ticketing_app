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
import NextImage from '~/components/ui/img';

import { PhoneInput } from 'react-international-phone';
import 'react-international-phone/style.css';

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


  console.log("BREVO_SMS_API_KEY",process.env.BREVO_SMS_API_KEY)
  console.log("ADMIN",process.env.ADMIN)
  console.log("NEXT_PUBLIC_SITE_KEY",process.env.NEXT_PUBLIC_SITE_KEY)
  console.log("NEXT_PUBLIC_SECRET_KEY",process.env.NEXT_PUBLIC_SECRET_KEY)
  console.log("DATABASE_URL",process.env.DATABASE_URL)
  console.log("POSTGRES_URL",process.env.POSTGRES_URL)
  console.log("POSTGRES_URL_NON_POOLING",process.env.POSTGRES_URL_NON_POOLING)
  console.log("POSTGRES_PRISMA_URL",process.env.POSTGRES_PRISMA_URL)
  console.log("POSTGRES_USER",process.env.POSTGRES_USER)
  console.log("POSTGRES_HOST",process.env.POSTGRES_HOST)
  console.log("POSTGRES_PASSWORD",process.env.POSTGRES_PASSWORD)
  console.log("POSTGRES_DATABASE",process.env.POSTGRES_DATABASE)
  console.log("NEXT_PUBLIC_BASE_URL",process.env.NEXT_PUBLIC_BASE_URL)
  console.log("NEXT_PUBLIC_MEDIA_BASE_URL",process.env.NEXT_PUBLIC_MEDIA_BASE_URL)
  console.log("ACCESS_KEY",process.env.ACCESS_KEY)
  console.log("SECRET_KEY",process.env.SECRET_KEY)
  console.log("REGION",process.env.REGION)
  console.log("BUCKET_NAME",process.env.BUCKET_NAME)
  console.log("BREVO_EMAIL_API_KEY",process.env.BREVO_EMAIL_API_KEY)
  console.log("TOTAL_PROCESSING_BEARER",process.env.TOTAL_PROCESSING_BEARER)
  console.log("TOTAN_ENTITY_ID",process.env.TOTAN_ENTITY_ID)

  return (
    <section className="body-font pt-24 space-y-24">
      <div className="px-4 my-10 md:px-14 md:my-24 flex gap-14 w-full ">
        {/* <div className="w-2/5 mb-5 lg:mb-0 rounded-lg hidden lg:block  ">
          <SideImage
            image={ContactImage}
            text={'Connect with Us for '}
            text2={'Support, Questions'}
          />
        </div> */}
        <div className="pb-6 flex flex-col h-full  w-full    lg:w-3/5  mx-auto lg:mx-0  lg:text-left  rounded-none border-none  bg-card ">
          <div className="font-black  pt-4 pb-4">
            <p className="text-lg md:text-2xl pl-6 px-4 lg:px-8 font-bold">Contact Us</p>
            <hr className=" opacity-20 mt-4" />

            <div className="flex flex-col justify-start align-start pl-6 px-4 lg:px-8 gap-2 py-3">
              <div className="flex gap-3 items-center">
                <NextImage
                  className="object-contain"
                  src={location}
                  quality={100}
                  alt="Location"
                />
                <div className="text-grayColor text-lg font-[600]">Address</div>
              </div>
              <div className="flex flex-col text-grayColor text-sm font-thin">
                <span>Winnar LLC</span>
                <span>Office 16, 17th Floor,</span>
                <span>Centrum Business Lounge</span>
                <span>Bay Gate Tower, Business Bay,</span>
                <span>Dubai, UAE</span>
              </div>
            </div>
            <div className="flex flex-col justify-start align-start pl-6 px-4 lg:px-8 gap-2 py-3">
              <div className="flex gap-3 items-center">
                <NextImage
                  className="object-contain"
                  src={phone}
                  quality={100}
                  alt="Location"
                />
                <div className="text-grayColor text-lg font-[600]">
                  General enquiry
                </div>
              </div>
              <div className="flex flex-col text-grayColor text-sm font-thin">
                {/* <span className="mb-1">01386719064</span> */}
                <span>contact@winnar.com</span>
              </div>
            </div>
            <div className="flex flex-col justify-start align-start pl-6 px-4 lg:px-8 gap-2 py-3">
              <div className="flex gap-3 items-center">
                <NextImage
                  className="object-contain"
                  src={mail}
                  quality={100}
                  alt="Location"
                />
                <div className="text-grayColor text-lg font-[600]">
                  Sell your car
                </div>
              </div>
              <div className="text-grayColor text-sm font-thin">
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
                          {...field}
                        />
                      </FormControl>

                      <div className="relative pb-2 errormsg">
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
                          {...field}
                        />
                      </FormControl>

                      <div className="relative pb-2 errormsg">
                        <FormMessage />
                      </div>
                    </FormItem>
                  )}
                />

                <div className="space-y-2 w-full">
                  <FormLabel className="text-xs  font-thin text-grayColor">
                    Phone Number*
                  </FormLabel>
                  <div className="flex items-start gap-2">
                    <FormField
                      control={form.control}
                      name="code"
                      render={({ field }) => (
                        <FormItem>
                          <PhoneInput
                            className="rounded-md countrycode"
                            defaultCountry="ae"
                            inputProps={{ maxLength: 4, placeholder: "+971", readOnly: true, ...field }} 
                            {...field} 
                          /> 
                          <div className="relative pb-2 errormsg">
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
                  control={form.control}
                  name="message"
                  render={({ field }) => (
                    <FormItem className="mb-4 ">
                      <FormLabel className="text-xs  font-thin text-grayColor">
                        Message*
                      </FormLabel>
                      <FormControl className="rounded-md bg-inputColor">
                        <Textarea
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
