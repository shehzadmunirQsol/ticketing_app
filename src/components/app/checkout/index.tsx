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
import { CheckoutDialog } from '~/components/common/modal/checkout';
import { CreateCheckoutSchema, createCheckoutSchema } from '~/schema/order';
import { zodResolver } from '@hookform/resolvers/zod';
import { trpc } from '~/utils/trpc';
import { useRouter } from 'next/router';
import { LoadingDialog } from '~/components/common/modal/loadingModal';
import { useToast } from '~/components/ui/use-toast';
import { addCart } from '~/store/reducers/cart';
import Link from 'next/link';
import { userAuth } from '~/store/reducers/auth';

function Checkout() {
  const { cart, totalAmount } = useSelector((state: RootState) => state.cart);
  const { user } = useSelector((state: RootState) => state.auth);
  const router = useRouter();
  const { toast } = useToast();
  const dispatch = useDispatch();

  // Handle Coupon Dailog
  const [loading, setLoading] = useState<boolean>(false);

  const [isModal, setIsModal] = useState(false);
  const [selectedItem, setSelectedItem] = useState({});
  const [title, setTitle] = useState('Enter Payment Detail');
  const [type, setType] = useState('');
  const [isCardModal, setIsCardModal] = useState(false);

  // 1. Define your form.
  const form = useForm<CreateCheckoutSchema>({
    resolver: zodResolver(createCheckoutSchema),
    defaultValues: {
      cart_id: cart.id ?? 0,
      customer_id: cart.customer_id ?? 0,
      first_name: user?.first_name,
      last_name: user?.last_name,
      code: '+971',
      country: 'United Arab Emirates',
      state: 'Abu Dhabi',

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
      form.setValue('dob',user?.dob ? user?.dob?.toISOString().split('T')[0] ?? '');
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
        // form.setValue(
        //   'country',
        //   user?.CustomerAddress[0]?.country ?? 'United Arab Emirates',
        // );
        form.setValue(
          'phone_number',
          user?.CustomerAddress[0]?.phone_number ?? '',
        );
        form.setValue(
          'postal_code',
          user?.CustomerAddress[0]?.postal_code?.toString() ?? '',
        );
        // form.setValue('state', user?.CustomerAddress[0]?.state ?? 'Abu Dhabi');
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
      console.log(data?.checkout?.data?.id, 'get checkout id');
      if (data?.checkout?.data) {
        setIsCardModal(true);
        setSelectedItem({
          values: { ...values },
          checkoutID: data?.checkout?.data?.id,
        });
      }
    } catch (err) {
      setIsCardModal(false);
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
                          Street Address <sup className="text-red-500">*</sup>
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
                        <FormItem>
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
                              <SelectTrigger className=" rounded-none  ">
                                <SelectValue placeholder="Select your country" />
                              </SelectTrigger>
                            </FormControl>
                            <SelectContent>
                              <SelectGroup>
                                {countries?.map((item) => (
                                  <SelectItem
                                    key={item.country}
                                    value={item.country}
                                  >
                                    {item?.country?.toUpperCase()}
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
                          <Select
                            onValueChange={field.onChange}
                            defaultValue={field.value}
                            value={field.value}
                          >
                            <FormControl>
                              <SelectTrigger className=" rounded-none  ">
                                <SelectValue placeholder="Select your state" />
                              </SelectTrigger>
                            </FormControl>
                            <SelectContent>
                              <SelectGroup>
                                {states?.map((item) => (
                                  <SelectItem
                                    key={item.state}
                                    value={item.state}
                                  >
                                    {item?.state?.toUpperCase()}
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
                    <p className="text-sm text-cardGray  mb-3 ">Phone Number</p>
                    <div className="flex flex-row gap-2 ">
                      <FormField
                        control={form.control}
                        name="code"
                        render={({ field }) => (
                          <FormItem>
                            <Select
                              onValueChange={field.onChange}
                              defaultValue={field.value}
                              value={field.value}
                            >
                              <FormControl>
                                <SelectTrigger className=" h-10  ">
                                  <SelectValue placeholder="+971" />
                                </SelectTrigger>
                              </FormControl>
                              <SelectContent>
                                <SelectGroup>
                                  {countryCode?.map((item) => (
                                    <SelectItem
                                      key={item.code}
                                      value={item.code}
                                    >
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
                        name="phone_number"
                        render={({ field }) => (
                          <FormItem className=" w-full">
                            {/* <FormLabel className="text-sm text-cardGray">
                            Email  <sup className="text-red-500">*</sup>
                          </FormLabel> */}
                            <FormControl className="rounded-md bg-inputColor">
                              <Input
                                max={999999999}
                                type="number"
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
                            Date of Birth <sup className="text-red-500">*</sup>
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
                    ? cart?.cartItems?.map((item) => {
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
                              {(item?.Event?.price * item?.quantity)?.toFixed(
                                2,
                              )}
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
                      <p className="text-xl">AED {totalAmount?.toFixed(2)}</p>
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
                  <p className="lg:text-2xl md:lg:text-xl font-black">Total:</p>
                  <p className="font-black text-lg lg:text-xl text-primary">
                    AED {(totalAmount - discountAmount)?.toFixed(2)}
                  </p>
                </div>
                <p className="lg:text-base md:text-sm text-sm text-cardGray md:w-[65%] lg:w-[85%]">
                  Your personal data will be used to process your order, support
                  your experience throughout this website, and for other
                  purposes described in our{' '}
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
                    I’m 18 years old or over and i have read and agree to the
                    website
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
      <CheckoutDialog
        selectedItem={selectedItem}
        setSelectedItem={setSelectedItem}
        title={title}
        setTitle={setTitle}
        isModal={isCardModal}
        setIsModal={setIsCardModal}
        type={type}
        setType={setType}
      />
    </div>
  );
}

export default Checkout;

const countries = [
  {
    country: 'United Arab Emirates',
  },
];

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
