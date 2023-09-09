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
import { useState } from 'react';
import { useSelector } from 'react-redux';
import { RootState } from '~/store/store';
import { CheckoutDialog } from '~/components/common/modal/checkout';
import { CreateCheckoutSchema, createCheckoutSchema } from '~/schema/order';
import { zodResolver } from '@hookform/resolvers/zod';

function Checkout() {
  const { cart, totalAmount } = useSelector((state: RootState) => state.cart);
  const { user } = useSelector((state: RootState) => state.auth);

  // Handle Coupon Dailog
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
      apartment: user?.apartment,
      street_address: user?.street_address,
      city: user?.city,
      code: '+971',
      country: user?.country,
      dob: user?.dob,
      email: user?.email,
      phone_number: user?.phone_number,
      postal_code: user?.postal_code,
      state: user?.state,
    },
  });

  const onSubmitCheckout = async (values: any) => {
    try {
      setIsCardModal(true);
      setSelectedItem({ ...values });
    } catch (err) {
      setIsCardModal(false);
    }
  };

  const discountAmount = cart.isPercentage
    ? totalAmount * (cart.discount / 100)
    : cart.discount;

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
                        <FormMessage />
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
                        <FormMessage />
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
                        <FormMessage />
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
                        <FormMessage />
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

                          <FormMessage />
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

                          <FormMessage />
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
                        <FormMessage />
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
                        <FormMessage />
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
                      <FormMessage />
                    </FormItem>
                  )}
                />

                <div className="flex flex-col items-center   md:flex-row gap-2  w-full justify-between">
                  <div className=" w-full ">
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
                                <SelectTrigger className=" rounded-none  ">
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

                            <FormMessage />
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
                                maxLength={9}
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
                  <div className="flex-1 w-full ">
                    <FormField
                      control={form.control}
                      name="dob"
                      render={() => (
                        <FormItem className=" w-full">
                          <FormLabel className="text-sm text-cardGray">
                            Date of Birth <sup className="text-red-500">*</sup>
                          </FormLabel>
                          <FormControl className="rounded-md bg-inputColor">
                            <Input
                              type="date"
                              placeholder="Enter your email address"
                              {...form.register('dob', { valueAsDate: true })}
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
            <div className=" flex-[0.45] space-y-12 z-10">
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
              <div className="space-y-8">
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
                <p className="lg:text-base md:text-sm text-sm text-cardGray">
                  Your personal data will be used to process your order, support
                  your experience throughout this website, and for other
                  purposes described in our{' '}
                  <span className="text-white"> privacy policy</span>.
                </p>
                <div className="flex flex-row gap-2 justify-start w-full  lg:w-[60%] md:w-[60%] items-center">
                  <div>
                    <Input type="checkbox" className="accent-white text-2xl " />
                  </div>
                  <p className="text-sm text-cardGray">
                    I’m 18 years old or over and i have read and agree to the
                    website
                    <span className="text-white"> Terms & Conditions</span>.
                  </p>
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
              <Glow className=" absolute bottom-[750px]  lg:bottom-[440px] md:bottom-[440px] -right-16  w-1/5 h-[350px] -z-2 opacity-50 " />

              <Button
                className=" px-16 mt-10 w-full  text-black font-sans font-[900]   text-xl tracking-[-1px]"
                variant="clip"
              >
                PAY WITH CARD
              </Button>
            </div>
          </div>
        </form>
      </Form>
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
    state: 'United Arab Emirates',
  },
];

const countryCode = [
  {
    code: '+971',
  },
];
