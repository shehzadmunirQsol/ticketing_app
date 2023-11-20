import { useRouter } from 'next/router';
import { Button } from '~/components/ui/button';
import { Input } from '~/components/ui/input';
import {
  Dialog,
  DialogContent,
  DialogDescription,
  DialogHeader,
  DialogTitle,
} from '~/components/ui/dialog';
import { useToast } from '~/components/ui/use-toast';
import { trpc } from '~/utils/trpc';
import OtpImage from '~/public/assets/otp-screen.svg';
import { useRef, useEffect, useState } from 'react';
import { useDispatch, useSelector } from 'react-redux';
import { userAuth } from '~/store/reducers/auth';
import { LoadingDialog } from '~/components/common/modal/loadingModal';
import langContent from '~/locales';
import { RootState } from '~/store/store';
import NextImage from '~/components/ui/img';

interface OtpVerificationDailogInterface {
  otpIsModal: boolean;
  setOtpIsModal: (e: any) => void;
  emailOrUser: string;
}
export function OtpVerificationDailog(props: OtpVerificationDailogInterface) {
  const { lang } = useSelector((state: RootState) => state.layout);

  const { toast } = useToast();
  const router = useRouter();
  const dispatch = useDispatch();

  const inputOne: any = useRef<HTMLInputElement>(null);
  const inputTwo: any = useRef<HTMLInputElement>(null);
  const inputThree: any = useRef<HTMLInputElement>(null);
  const inputFour: any = useRef<HTMLInputElement>(null);

  const [seconds, setSeconds] = useState(60);
  const [showTimer, setShowTimer] = useState(true);

  useEffect(() => {
    const intervalId = setInterval(() => {
      if (seconds > 0) {
        setSeconds(seconds - 1);
      } else {
        clearInterval(intervalId);
        setShowTimer(false);
      }
    }, 1000);
    return () => clearInterval(intervalId);
  }, [seconds]);

  // Response OTP Verification
  const otpVerification = trpc.customer.verificationOtpCustomer.useMutation({
    onSuccess: (res) => {
      toast({
        variant: 'success',
        title: 'Login successfully',
      });

      dispatch(userAuth(res?.user));
      localStorage.setItem('winnar-token', res?.jwt);
      props.setOtpIsModal(false);
      router.replace('/');
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
      toast({
        variant: 'success',
        title: 'Please check your email',
      });
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

  // Handle Forgot Password
  function inputChangeHandler(event: React.ChangeEvent<HTMLInputElement>) {
    const { name } = event.target;

    switch (name) {
      case 'otp_1':
        // check if number
        const patternInputOne = isNumber(inputOne?.current?.value);
        if (patternInputOne) {
          inputTwo.current?.focus();
        } else {
          if (inputOne.current) inputOne.current.value = '';
        }

        break;
      case 'otp_2':
        // check if number
        const patternInputTwo = isNumber(inputTwo?.current?.value);
        if (patternInputTwo) {
          inputThree.current?.focus();
        } else {
          if (inputTwo.current) inputTwo.current.value = '';
        }

        break;
      case 'otp_3':
        // check if number
        const patternInputThree = isNumber(inputThree?.current?.value);
        if (patternInputThree) {
          inputFour.current?.focus();
        } else {
          if (inputThree.current) inputThree.current.value = '';
        }

        break;
      case 'otp_4':
        // check if number
        const patternInputFour = isNumber(inputFour?.current?.value);
        if (patternInputFour) {
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
    try {
      event.preventDefault();

      const result: any = {
        emailOrUser: props?.emailOrUser,
        otp_1: +inputOne.current.value,
        otp_2: +inputTwo.current.value,
        otp_3: +inputThree.current.value,
        otp_4: +inputFour.current.value,
      };

      await otpVerification.mutateAsync(result);
    } catch (error: any) {
      toast({
        variant: 'destructive',
        title: error?.message,
      });
    }
  }

  async function handleResendOtp() {
    await resendOtpCustomer.mutateAsync({
      emailOrUser: props?.emailOrUser,
    });
  }

  return (
    <>
      <Dialog
        open={props?.otpIsModal}
        onOpenChange={(e: any): any => props.setOtpIsModal(e)}
      >
        <DialogContent className="  bg-gradient-to-b from-[#444E5566] via-gray-[#3841471A] to-transparent text-center py-8 ">
          <DialogHeader>
            <DialogTitle className="text-center ">
              <p className="font-light text-2xl">
                {langContent[lang.lang].Auth.OTPSCREEN.HEADING}
              </p>
              <p className="font-bold text-2xl">
                {langContent[lang.lang].Auth.OTPSCREEN.SUB_HEADING}
              </p>
              <div className="flex items-center justify-center my-6">
                <NextImage
                  src={OtpImage}
                  alt="otpImage"
                  className="max-w-full"
                />
              </div>
            </DialogTitle>
            <DialogDescription>
              <form
                onSubmit={onSubmit}
                className="justify-center items-center px-2 lg:px-8 py-4 space-y-4"
              >
                <p className="text-center text-grayColor">
                  {langContent[lang.lang].Auth.OTPSCREEN.TEXT}
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
                <div className="flex flex-row justify-center items-center  ">
                  <p className="text-center text-grayColor text-xs pr-4  cursor-pointer">
                    {langContent[lang.lang].Auth.OTPSCREEN.OTP}{' '}
                  </p>
                  <button
                    disabled={showTimer}
                    onClick={handleResendOtp}
                    type="button"
                    className="text-white text-xs underline cursor-pointer"
                  >
                    {langContent[lang.lang].Auth.OTPSCREEN.RESEND}{' '}
                    {seconds ? seconds + 's' : ''}
                  </button>
                </div>
                <div className="w-full mx-auto">
                  <div className=" flex items-center justify-center">
                    <Button
                      className="align-center uppercase rounded-full px-10   text-black font-sans font-[900]   text-xl tracking-[-1px]"
                      variant="clip"
                    >
                      {langContent[lang.lang].Auth.OTPSCREEN.BUTTON}
                    </Button>
                  </div>
                </div>
              </form>
            </DialogDescription>
          </DialogHeader>
        </DialogContent>
      </Dialog>
      <LoadingDialog
        open={resendOtpCustomer.isLoading || otpVerification?.isLoading}
        text={'Loading...'}
      />
    </>
  );
}
