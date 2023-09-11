import { useRouter } from 'next/router';
import { Button } from '~/components/ui/button';
import { Input } from '~/components/ui/input';
import {
  Dialog,
  DialogContent,
  DialogDescription,
  DialogFooter,
  DialogHeader,
  DialogTitle,
  DialogTrigger,
} from '~/components/ui/dialog';
import {
  Form,
  FormControl,
  FormDescription,
  FormField,
  FormItem,
  FormLabel,
  FormMessage,
} from '@/ui/form';
import { useForm } from 'react-hook-form';
import { useToast } from '~/components/ui/use-toast';
import { trpc } from '~/utils/trpc';
import OtpImage from '~/public/assets/otp-screen.svg';
import Image from 'next/image';
import { useRef, useEffect, useState } from 'react';
interface OtpVerificationDailogInterface {
  otpIsModal: boolean;
  setOtpIsModal: (e: any) => void;
}
export function OtpVerificationDailog(props: OtpVerificationDailogInterface) {
  const { toast } = useToast();
  const router = useRouter();
  const inputOne: any = useRef<HTMLInputElement>(null);
  const inputTwo: any = useRef<HTMLInputElement>(null);
  const inputThree: any = useRef<HTMLInputElement>(null);
  const inputFour: any = useRef<HTMLInputElement>(null);

  
  // Response OTP Verification
  const otpVerification = trpc.customer.verificationOtpCustomer.useMutation({
    onSuccess: (res: any) => {
      console.log(res, 'mein hun res');
      toast({
        variant: 'success',
        title: 'Your Otp is Verified please wait admin verification ',
      });
      props.setOtpIsModal(false);
      router.push('/login');
    },
    onError: (err) => {
      console.log(err.message, 'err');
      toast({
        variant: 'destructive',
        title: err.message,
      });
    },
  });

  // Response OTP Verification
  const resendOtpCustomer = trpc.customer.resendOtpCustomer.useMutation({
    onSuccess: (res: any) => {
      console.log(res, 'mein hun res');
      toast({
        variant: 'success',
        title: 'Please check your email',
      });
      // props.setOtpIsModal(false)
      // router.push('/login');
    },
    onError: (err) => {
      console.log(err.message, 'err');
      toast({
        variant: 'destructive',
        title: err.message,
      });
    },
  });

  // check isNumber or not
  const isNumber = (value: any) => {
    const pattern = /^-?\d+(\.\d+)?$/;
    return pattern.test(value);
  };
  // console.log(user, 'State');

  // Handle Forgot Password
  function inputChangeHandler(event: React.ChangeEvent<HTMLInputElement>) {
    const { name, value } = event.target;

    switch (name) {
      case 'otp_1':
        // check if number
        const pattrenInputOne = isNumber(inputOne?.current?.value);
        if (pattrenInputOne) {
          inputTwo.current?.focus();
        } else {
          if (inputOne.current) inputOne.current.value = '';
        }

        break;
      case 'otp_2':
        // check if number
        const pattrenInputTwo = isNumber(inputTwo?.current?.value);
        if (pattrenInputTwo) {
          inputThree.current?.focus();
        } else {
          if (inputTwo.current) inputTwo.current.value = '';
        }

        break;
      case 'otp_3':
        // check if number
        const pattrenInputThree = isNumber(inputThree?.current?.value);
        if (pattrenInputThree) {
          inputFour.current?.focus();
        } else {
          if (inputThree.current) inputThree.current.value = '';
        }

        break;
      case 'otp_4':
        // check if number
        const pattrenInputFour = isNumber(inputFour?.current?.value);
        if (pattrenInputFour) {
          inputFour.current?.focus();
        } else {
          if (inputFour.current) inputFour.current.value = '';
        }

        break;

      default:
        break;
    }
  }

  async function onSubmit(event: React.FormEvent<HTMLFormElement>) {
    event.preventDefault();

    const storedData: string | null = localStorage?.getItem('customer');

    if (storedData !== null) {
      const userData: any = JSON.parse(storedData);
      const result: any = {
        email: userData.email,
        otp_1: +inputOne.current.value,
        otp_2: +inputTwo.current.value,
        otp_3: +inputThree.current.value,
        otp_4: +inputFour.current.value,
      };
      console.log(result, 'otp Hun mein ');
      const otpResult = await otpVerification.mutateAsync(result);
      console.log(otpResult, 'otpResult');
      console.log(otpResult, 'otpResult');
    }
  }

  async function handleResendOtp() {
    const storedData: string | null = localStorage?.getItem('customer');

    if (storedData !== null) {
      const userData: any = JSON.parse(storedData);
      const otpResult = await resendOtpCustomer.mutateAsync({
        email: userData.email,
      });
      console.log(otpResult, 'otpResult');
    }
  }

  return (
    <Dialog
      open={props?.otpIsModal}
      onOpenChange={(e: any): any => props.setOtpIsModal(e)}
    >
      <DialogContent className="  bg-gradient-to-b from-[#444E5566] via-gray-[#3841471A] to-transparent text-center py-8 ">
        <DialogHeader>
          <DialogTitle className="text-center ">
            <p className="font-light text-2xl">OTP</p>
            <p className="font-bold text-2xl">VERIFICATION</p>
            <div className="flex items-center justify-center my-6">
              <Image src={OtpImage} alt="otpImage" className="max-w-full" />
            </div>
          </DialogTitle>
          <DialogDescription>
            <form
              onSubmit={onSubmit}
              className="justify-center items-center px-2 lg:px-8 py-4 space-y-4"
            >
              <p className="text-center text-grayColor">
                We have sent OTP on your email
              </p>
              <div className="flex gap-4 mb-2">
                <Input
                  name="otp_1"
                  maxLength={1}
                  type="text"
                  ref={inputOne}
                  onChange={inputChangeHandler}
                  className="bg-transparent text-center py-8 "
                />
                <Input
                  name="otp_2"
                  maxLength={1}
                  type="text"
                  ref={inputTwo}
                  onChange={inputChangeHandler}
                  className="bg-transparent text-center py-8 "
                />
                <Input
                  name="otp_3"
                  maxLength={1}
                  type="text"
                  ref={inputThree}
                  onChange={inputChangeHandler}
                  className="bg-transparent text-center py-8 "
                />
                <Input
                  name="otp_4"
                  maxLength={1}
                  type="text"
                  ref={inputFour}
                  onChange={inputChangeHandler}
                  className="bg-transparent text-center py-8 "
                />
              </div>
              <div
                className="flex flex-row justify-center items-center  "
                onClick={handleResendOtp}
              >
                <p className="text-center text-grayColor text-xs pr-4 underline cursor-pointer">
                  Didn’t receive an OTP?{' '}
                </p>
                <p className="text-white text-xs underline cursor-pointer">Resend OTP</p>
              </div>
              <div className="w-full mx-auto">
                <div className=" flex items-center justify-center">
                  <Button
                    className="align-center  rounded-full px-10   text-black font-sans font-[900]   text-xl tracking-[-1px]"
                    variant="clip"
                  >
                    Enter
                  </Button>
                </div>
              </div>
            </form>
          </DialogDescription>
        </DialogHeader>
      </DialogContent>
    </Dialog>
  );
}
