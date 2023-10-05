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
import { CouponModal } from './Coupon';
import { useEffect, useState } from 'react';
import { useDispatch, useSelector } from 'react-redux';
import { RootState } from '~/store/store';
import { CreateCheckoutSchema, createCheckoutSchema } from '~/schema/order';
import { zodResolver } from '@hookform/resolvers/zod';
import { trpc } from '~/utils/trpc';
import { useRouter } from 'next/router';
import { LoadingDialog } from '~/components/common/modal/loadingModal';
import { useToast } from '~/components/ui/use-toast';
import { addCart } from '~/store/reducers/cart';
import Link from 'next/link';
import Script from 'next/script';
import jqeury from 'jquery';
import { CardDailog } from '~/components/common/modal/cardModal';

import countryJSON from '~/data/countries.json';
import { Check } from 'lucide-react';
import { cn } from '~/utils/cn';
const countries = countryJSON.map((item) => item.country);

function Checkout() {
  const { cart, totalAmount } = useSelector((state: RootState) => state.cart);
  const { user } = useSelector((state: RootState) => state.auth);
  const router = useRouter();
  const { toast } = useToast();
  const dispatch = useDispatch();

  // Handle Coupon Dailog
  const [loading, setLoading] = useState<boolean>(false);
  const [totalID, setTotalID] = useState<any>(null);
  const [countryCombobox, setCountryCombobox] = useState(false);
  const [selectCountry, setSelectCountry] = useState('');

  const [isModal, setIsModal] = useState(false);
  const [isDeleteModal, setIsDeleteModal] = useState(false);
  const [index, setIndex] = useState(null);
  const [selectedItem, setSelectedItem] = useState({});
  const [type, setType] = useState('');
  // 1. Define your form.
  const form = useForm<CreateCheckoutSchema>({
    resolver: zodResolver(createCheckoutSchema),
    defaultValues: {
      cart_id: cart.id ?? 0,
      customer_id: cart.customer_id ?? 0,
      first_name: user?.first_name,
      last_name: user?.last_name,
      code: user?.code,
      country: user?.country,
      state: '',

      dob: user?.dob,
      email: user?.email,
      phone_number: user?.phone_number,
    },
  });
  const checkoutCreator = trpc.order.createCheckout.useMutation({
    onSuccess: () => {
      console.log('upload successfully');
    },
    onError(error: any) {
      console.log({ error });
    },
  });
  useEffect(() => {
    if (user) {
      form.setValue('cart_id', cart?.id ?? 0);
      form.setValue('customer_id', cart?.customer_id ?? 0);
      form.setValue('first_name', user?.first_name ?? '');
      form.setValue('last_name', user?.last_name ?? '');
      form.setValue(
        'dob',
        user?.dob ? user?.dob?.toISOString().split('T')[0] : '',
      );
      form.setValue('email', user?.email ?? '');
      if (user?.CustomerAddress && user?.CustomerAddress?.length) {
        form.setValue(
          'apartment',
          user?.CustomerAddress[0]?.street_address_2 ?? '',
        );
        form.setValue(
          'street_address',
          user?.CustomerAddress[0]?.street_address_1 ?? '',
        );
        form.setValue('city', user?.CustomerAddress[0]?.city ?? '');
        form.setValue(
          'phone_number',
          user?.CustomerAddress[0]?.phone_number ?? '',
        );
        form.setValue(
          'code',
          user?.code ?? '',
        );
        form.setValue(
          'postal_code',
          user?.CustomerAddress[0]?.postal_code?.toString() ?? '',
        );
      }
    }
  }, [user, cart]);

  const getStatus = trpc.order.getStatus.useMutation({
    onSuccess: () => {
      console.log('upload successfully');
    },
    onError(error: any) {
      setLoading(false);

      console.log({ error });
    },
  });

  useEffect(() => {
    (async () => {
      try {
        const data = router?.query;
        if (data?.id) {
          setLoading(true);
          const Resdata = await getStatus.mutateAsync({
            checkout_id: data?.id as string,
          });
          if (Resdata?.status) {
            setTimeout(() => {
              router.push('/');
              toast({
                variant: 'success',
                title: 'Order Successful! 🎉',
              });
              dispatch(
                addCart({
                  id: null,
                  customer_id: null,
                  isDiscount: false,
                  discount: 0,
                  isPercentage: false,
                  cartItems: [],
                }),
              );
            }, 3000);
          }
        }
      } catch (e: any) {
        setLoading(false);
        router.push('/checkout');

        toast({
          variant: 'destructive',
          title: e?.message,
        });
        console.log(e?.message, 'error');
      }
    })();
  }, [router?.query]);

  const onSubmitCheckout = async (values: any) => {
    try {
      const data = await checkoutCreator.mutateAsync({
        values: {
          ...values,
          cart_id: cart?.id,
          customer_id: user?.id,
          total_id: user?.total_customer_id,
        },
      });
      if (data?.checkout?.data) {
        setTotalID(data?.checkout?.data?.id);
      }
    } catch (err: any) {
      toast({
        variant: 'destructive',
        title: err?.message,
      });
    }
  };

  const discountAmount = cart.isPercentage
    ? totalAmount * (cart.discount / 100)
    : cart.discount;

  const today = new Date();

  // Calculate the minimum date (18 years ago from today)
  const minDate = new Date(
    today.getFullYear() - 18,
    today.getMonth(),
    today.getDate(),
  );

  // Format the minimum date as "YYYY-MM-DD" for the input field
  const minDateFormatted = minDate.toISOString().split('T')[0];

  return (
    <div className="relative mt-20 bg-background py-6 px-4 space-y-10 md:py-16 md:px-14 md:space-y-14">
      {totalID ? (
        <>
          <Script
            src={`https://eu-test.oppwa.com/v1/paymentWidgets.js?checkoutId=${totalID}`}
            onReady={() => {
              console.log('Script has loaded');
              const wpwlOptions = {
                registrations: {
                  requireCvv: true,
                },
              };

              function addCustomElement() {
                // Create the HTML elements

                const createRegistrationHtml =
                  '<div class="customLabel">Store payment details?</div><div class="customInput"><input type="checkbox" name="createRegistration" value="true" /></div>';
                jqeury('form.wpwl-form-card')
                  .find('.wpwl-button')
                  .before(createRegistrationHtml);
                const createDeleteOption = `<div class="customLabel flex-1 relative w-full  ml-auto flex justify-end items-center"><i  class="fa-regular border border-border deletefunction p-2 rounded-full fa-trash-can  text-cardGray cursor-pointer hover:text-white hover:border-white "></i></div>`;
                jqeury('.wpwl-group-registration')
                  .find('.wpwl-registration')
                  .after(createDeleteOption);

                jqeury('.deletefunction').click(function (event) {
                  console.log('i am running');
                  setIsDeleteModal(true);
                  setIndex(jqeury('.deletefunction').index(this) as any);
                });
              }

              setTimeout(addCustomElement, 4000);
            }}
          ></Script>
          <div className=" relative  bg-background   ">
            <h2 className="lg:text-4xl md:text-4xl text-2xl font-black uppercase mb-6">
              Payment
            </h2>
            <form
              action={`${process.env.NEXT_PUBLIC_BASE_URL}/checkout`}
              className="paymentWidgets justify-start   lg:justify-center md:justify-center items-center px-2 lg:px-6 py-2 space-y-2 text-black"
              data-brands="VISA MASTER AMEX "
            ></form>
          </div>
        </>
      ) : (
        <>
          <Form {...form}>
            <form
              onSubmit={form.handleSubmit(onSubmitCheckout)}
              className="justify-center items-center  py-4"
            >
              <h2 className="lg:text-5xl md:text-4xl text-2xl font-black uppercase mb-6">
                Checkout
              </h2>
              <div className="flex flex-col gap-8 lg:flex-row md:flex-row justify-between w-full ">
                <div className="flex-[0.55] space-y-6">
                  <h3 className="text-lg md:text-xl lg:text-2xl font-bold ">
                    Billing Details
                  </h3>
                  <div className="space-y-6">
                    <div className="flex flex-col lg:flex-row md:flex-row gap-2  w-full justify-between">
                      <FormField
                        control={form.control}
                        name="first_name"
                        render={({ field }) => (
                          <FormItem className=" w-full ">
                            <FormLabel className="text-sm text-cardGray ">
                              Name <sup className="text-red-500">*</sup>
                            </FormLabel>
                            <FormControl className="rounded-md bg-inputColor">
                              <Input
                                type="text"
                                placeholder="Enter your name"
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
                        name="last_name"
                        render={({ field }) => (
                          <FormItem className=" w-full ">
                            <FormLabel className="text-sm text-cardGray">
                              Last Name <sup className="text-red-500">*</sup>
                            </FormLabel>
                            <FormControl className="rounded-md bg-inputColor">
                              <Input
                                type="text"
                                placeholder="Enter last name"
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

                    <div className="flex flex-col gap-y-3">
                      <FormField
                        control={form.control}
                        name="street_address"
                        render={({ field }) => (
                          <FormItem className="w-full ">
                            <FormLabel className="text-sm text-cardGray">
                              Street Address{' '}
                              <sup className="text-red-500">*</sup>
                            </FormLabel>
                            <FormControl className="rounded-md bg-inputColor">
                              <Input
                                type="text"
                                placeholder="House number and street name"
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
                        name="apartment"
                        render={({ field }) => (
                          <FormItem className="w-full ">
                            <FormControl className="rounded-md bg-inputColor">
                              <Input
                                type="text"
                                placeholder="Apartment, suit, unit etc. (Optional) "
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

                    <div className="flex flex-col lg:flex-row md:flex-row gap-2  w-full justify-between">
                      <div className="w-full ">
                        <FormField
                          control={form.control}
                          name="country"
                          render={({ field }) => (
                            <FormItem className="">
                              <FormLabel className="text-sm text-cardGray">
                                Country/ Region{' '}
                                <sup className="text-red-500">*</sup>
                              </FormLabel>
                              <Select
                                onValueChange={field.onChange}
                                defaultValue={field.value}
                                value={field.value}
                              >
                                <FormControl>
                                  <SelectTrigger className="h-10 rounded-md  bg-inputColor">
                                    <SelectValue placeholder="Select your country" />
                                  </SelectTrigger>
                                </FormControl>
                                <SelectContent className="max-h-[300px] overflow-y-auto">
                                  <SelectGroup>
                                    {countries &&
                                      countries?.map((country, i) => {
                                        return (
                                          <SelectItem key={i} value={country}>
                                            {country?.toUpperCase()}
                                          </SelectItem>
                                        );
                                      })}
                                  </SelectGroup>
                                </SelectContent>
                              </Select>

                              <div className="relative pb-2">
                                <FormMessage />
                              </div>
                            </FormItem>
                          )}
                        />
                      </div>
                      <div className="w-full ">
                        <FormField
                          control={form.control}
                          name="state"
                          render={({ field }) => (
                            <FormItem>
                              <FormLabel className="text-sm text-cardGray">
                                State <sup className="text-red-500">*</sup>
                              </FormLabel>
                              <FormControl className="rounded-md bg-inputColor">
                                <Input
                                  type="text"
                                  placeholder="Enter the state"
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
                    <div className="flex flex-col lg:flex-row md:flex-row gap-2  w-full justify-between">
                      <FormField
                        control={form.control}
                        name="city"
                        render={({ field }) => (
                          <FormItem className=" w-full ">
                            <FormLabel className="text-sm text-cardGray">
                              Town/City <sup className="text-red-500">*</sup>
                            </FormLabel>
                            <FormControl className="rounded-md bg-inputColor">
                              <Input
                                type="text"
                                placeholder="Enter the city"
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
                        name="postal_code"
                        render={({ field }) => (
                          <FormItem className=" w-full ">
                            <FormLabel className="text-sm text-cardGray">
                              Postcode <sup className="text-red-500">*</sup>
                            </FormLabel>
                            <FormControl className="rounded-md bg-inputColor">
                              <Input
                                type="text"
                                placeholder="Enter your postcode"
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
                    <FormField
                      control={form.control}
                      name="email"
                      render={({ field }) => (
                        <FormItem className=" w-full">
                          <FormLabel className="text-sm text-cardGray">
                            Email <sup className="text-red-500">*</sup>
                          </FormLabel>
                          <FormControl className="rounded-md bg-inputColor">
                            <Input
                              type="email"
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

                    <div className="flex items-start flex-col lg:flex-row gap-2 w-full justify-between">
                      <div className="w-full">
                        <p className="text-sm text-cardGray  mb-2 ">
                          Phone Number
                        </p>
                        <div className="flex flex-row gap-2 ">
                          <FormField
                            control={form.control}
                            name="code"
                            render={({ field }) => (
                              <FormItem>
                                <Input
                                  type="text"
                                  className="rounded-md w-20 bg-inputColor"
                                  placeholder="+971"
                                  maxLength={5}
                                  {...field}
                                />
                                <div className="relative pb-2">
                                  <FormMessage />
                                </div>
                              </FormItem>
                            )}
                          />
                          <FormField
                            control={form.control}
                            name="phone_number"
                            render={({ field }) => (
                              <FormItem className=" w-full">
                                {/* <FormLabel className="text-sm text-cardGray">
                            Email  <sup className="text-red-500">*</sup>
                          </FormLabel> */}
                                <FormControl className="rounded-md bg-inputColor">
                                  <Input
                                    type="text"
                                    className="w-full"
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
                      <div className="w-full">
                        <FormField
                          control={form.control}
                          name="dob"
                          render={() => (
                            <FormItem>
                              <FormLabel className="text-sm text-cardGray">
                                Date of Birth{' '}
                                <sup className="text-red-500">*</sup>
                              </FormLabel>
                              <FormControl className="rounded-md bg-inputColor">
                                <Input
                                  max={minDateFormatted}
                                  type="date"
                                  placeholder="Enter your Date of Birth"
                                  {...form.register('dob', {
                                    valueAsDate: true,
                                    required: 'required',
                                  })}
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
                <div className="relative bg-background  flex-[0.45] space-y-12 z-10">
                  <div className="flex flex-row justify-between items-center">
                    <h3 className="text-lg md:text-xl lg:text-2xl font-bold">
                      Order Summary
                    </h3>
                    {!cart.isDiscount ? (
                      <p
                        className="text-white/40 text-sm lg:text-base cursor-pointer"
                        onClick={() => setIsModal(true)}
                      >
                        Have a coupon code?
                      </p>
                    ) : null}
                  </div>
                  <div className="relative space-y-8">
                    <div className=" max-h-60 overflow-x-auto space-y-8">
                      {cart?.cartItems?.length
                        ? cart?.cartItems?.map((item: any) => {
                            return (
                              <div
                                className="flex flex-row justify-between "
                                key={item.id}
                              >
                                <p className="lg:text-2xl md:lg:text-xl   w-[60%]">
                                  {item?.Event?.EventDescription[0]?.name}
                                </p>
                                <p className="font-black text-lg lg:text-xl ">
                                  AED{' '}
                                  {(
                                    item?.Event?.price * item?.quantity
                                  )?.toFixed(2)}
                                </p>
                              </div>
                            );
                          })
                        : null}
                    </div>
                    {cart?.isDiscount ? (
                      <div className="space-y-6">
                        <div className="h-[1px] bg-white/40" />

                        <div className="flex items-center justify-between z-10 ">
                          <p className="text-white/40  text-lg">Sub Total:</p>
                          <p className="text-xl">
                            AED {totalAmount?.toFixed(2)}
                          </p>
                        </div>
                        <div className="flex items-center justify-between z-10 ">
                          <p className="text-white/40  text-lg">Discount:</p>
                          <p className="text-xl">
                            {' '}
                            - AED {discountAmount?.toFixed(2)}
                          </p>
                        </div>
                      </div>
                    ) : null}

                    <div className="flex flex-row justify-between py-6 border-t border-b border-white/40">
                      <p className="lg:text-2xl md:lg:text-xl font-black">
                        Total:
                      </p>
                      <p className="font-black text-lg lg:text-xl text-primary">
                        AED {(totalAmount - discountAmount)?.toFixed(2)}
                      </p>
                    </div>
                    <p className="lg:text-base md:text-sm text-sm text-cardGray md:w-[65%] lg:w-[85%]">
                      Your personal data will be used to process your order,
                      support your experience throughout this website, and for
                      other purposes described in our{' '}
                      <span className="text-white">
                        {' '}
                        <Link href="/privacy-policy"> privacy policy </Link>
                      </span>
                      .
                    </p>
                    <div className="flex flex-row gap-2 justify-start   items-start w-full  md:w-[65%] lg:w-[85%]">
                      <input
                        id="terms-and-conditions"
                        type="checkbox"
                        className="accent-white  my-1"
                        required
                      />

                      <label
                        htmlFor="terms-and-conditions"
                        className="text-sm text-cardGray cursor-pointer"
                      >
                        I’m 18 years old or over and i have read and agree to
                        the website
                        <span className="text-white">
                          <Link href="/cms/terms-condition">
                            {' '}
                            Terms & Conditions{' '}
                          </Link>
                        </span>
                        .
                      </label>
                    </div>
                  </div>

                  <div className="flex flex-row gap-4 justify-center ">
                    <p className="text-sm text-cardGray">We accept</p>
                    <Image
                      className="w-64 object-contain  "
                      src={Group17}
                      quality={100}
                      alt="Sunset in the mountains"
                    />
                  </div>

                  <Button
                    disabled={checkoutCreator?.isLoading}
                    className="min-w-max w-full  px-16 mt-10  text-black font-sans font-[900]   text-xl tracking-[-1px]"
                    variant="clip"
                  >
                    PAY WITH CARD
                  </Button>

                  <Glow className=" absolute bottom-4   -right-16  w-2/6 h-72 -z-2  " />
                </div>
              </div>
            </form>
          </Form>
        </>
      )}
      <LoadingDialog
        open={loading || checkoutCreator?.isLoading}
        text={'Order processing...'}
      />

      <CouponModal
        isModal={isModal}
        setIsModal={setIsModal}
        customer_id={cart?.customer_id ?? 0}
        cart_id={cart?.id ?? 0}
      />

      <CardDailog
        selectedItem={selectedItem}
        setSelectedItem={setSelectedItem}
        isModal={isDeleteModal}
        setIsModal={setIsDeleteModal}
        type={type}
        setType={setType}
        totalID={totalID}
        setTotalID={setTotalID}
        index={index}
        setIndex={setIndex}
        values={form.getValues()}
      />
    </div>
  );
}

export default Checkout;

const states = [
  {
    state: 'Abu Dhabi',
  },
  {
    state: 'Dubai',
  },
  {
    state: 'Sharjah',
  },
  {
    state: 'Ajman',
  },
  {
    state: 'Umm Al-Quwain',
  },
  {
    state: 'Ras Al Khaimah',
  },
  {
    state: 'Fujairah',
  },
];

const countryCode = [
  {
    code: '+971',
  },
];
