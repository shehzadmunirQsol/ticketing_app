import { useState } from 'react';
import { Button } from '~/components/ui/button';
import {
  Dialog,
  DialogContent,
  DialogDescription,
  DialogFooter,
  DialogHeader,
  DialogTitle,
} from '~/components/ui/dialog';
import { useToast } from '~/components/ui/use-toast';
import { trpc } from '~/utils/trpc';
import { LoadingDialog } from './loadingModal';
import { useForm } from 'react-hook-form';
import {
  Form,
  FormControl,
  FormField,
  FormItem,
  FormLabel,
  FormMessage,
} from '~/components/ui/form';
import { Input } from '~/components/ui/input';
import {
  Select,
  SelectContent,
  SelectGroup,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from '~/components/ui/select';
import { createFormPaymentSchema, createPaymentSchema } from '~/schema/payment';
import { zodResolver } from '@hookform/resolvers/zod';
import { useDispatch, useSelector } from 'react-redux';
import { RootState } from '~/store/store';
import { z } from 'zod';
import { userAuth } from '~/store/reducers/auth';
import { addCart } from '~/store/reducers/cart';
import { useRouter } from 'next/router';
interface SettingDialogInterface {
  selectedItem: any;
  isModal: boolean;
  title: string;
  setTitle: any;
  setSelectedItem: any;
  setIsModal: any;
  type: string;
  setType: any;
}
export function CheckoutDialog(props: SettingDialogInterface) {
  const { user } = useSelector((state: RootState) => state.auth);
  const { cart, totalAmount } = useSelector((state: RootState) => state.cart);
  console.log({ user }, 'props?.selectedItem');

  console.log(user?.total_customer_id);
  const dispatch = useDispatch();
  const router = useRouter();

  const { toast } = useToast();
  const [loading, setLoading] = useState<boolean>(false);
  const formSchema = false ? createPaymentSchema : createFormPaymentSchema;
  console.log({ formSchema });
  const form = useForm<z.infer<typeof formSchema>>({
    resolver: zodResolver(
      // user?.total_customer_id ? createPaymentSchema : createFormPaymentSchema,
      false ? createPaymentSchema : createFormPaymentSchema,
    ),
  });

  const bannerUpdate = trpc.order.checkout.useMutation({
    onSuccess: () => {
      console.log('upload successfully');
    },
    onError(error: any) {
      console.log({ error });
    },
  });
  const onSubmit = async (values: z.infer<typeof formSchema>) => {
    try {
      setLoading(true);
      console.log(values, 'onSubmit');
      let payload: any = {
        // registrationId: user?.total_customer_id,
        customer_id: user?.id,
        values: {
          ...props?.selectedItem,
          cart_id:
            props?.selectedItem?.cart_id > 0
              ? props?.selectedItem?.cart_id
              : cart?.id,
          customer_id:
            props?.selectedItem?.customer_id > 0
              ? props?.selectedItem?.customer_id
              : user?.id,
        },
      };
      if (true) {
        payload = {
          ...values,
          registrationId: user?.total_customer_id,
          customer_id: user?.id,
          values: {
            ...props?.selectedItem,
            cart_id:
              props?.selectedItem?.cart_id > 0
                ? props?.selectedItem?.cart_id
                : cart?.id,
            customer_id:
              props?.selectedItem?.customer_id > 0
                ? props?.selectedItem?.customer_id
                : user?.id,
          },
        };
      }
      const data = await bannerUpdate.mutateAsync({
        ...payload,
      });
      if (data) {
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
        setLoading(false);

        router.push('/');
      }
    } catch (e: any) {
      setLoading(false);

      toast({
        variant: 'destructive',
        title: e?.message,
      });
    }
  };

  function formatCardNum(v: any) {
    form.setValue(
      'card.number',
      v
        .replace(/\s/g, '')
        .replace(/(\d{4})/g, '$1 ')
        .trim() as string,
    );
  }
  return (
    <>
      <Dialog open={props?.isModal} onOpenChange={(e) => props.setIsModal(e)}>
        <DialogContent className="sm:max-w-[450px]">
          <Form {...form}>
            <form
              onSubmit={form.handleSubmit(onSubmit)}
              className="justify-start  lg:justify-center md:justify-center items-center px-2 lg:px-6 py-2 space-y-2"
            >
              <DialogHeader>
                <DialogTitle>{props?.title}</DialogTitle>
                <DialogDescription className=" flex flex-col gap-2 ">
                  {false ? (
                    <div className="p-2 rounded-md text-center border mt-2">
                      Pay With Previous Bank Details
                    </div>
                  ) : (
                    <>
                      <FormField
                        control={form.control}
                        name="paymentBrand"
                        render={({ field }) => (
                          <FormItem>
                            <FormLabel className="text-xs font-thin text-grayColor">
                              Card Type
                            </FormLabel>
                            <Select
                              onValueChange={field.onChange}
                              defaultValue={field.value}
                              value={field.value}
                            >
                              <FormControl>
                                <SelectTrigger className=" rounded-none  ">
                                  <SelectValue placeholder="Select Card Type" />
                                </SelectTrigger>
                              </FormControl>
                              <SelectContent>
                                <SelectGroup>
                                  <SelectItem value={'VISA'}>VISA</SelectItem>
                                  <SelectItem value={'Master'}>
                                    Master
                                  </SelectItem>
                                </SelectGroup>
                              </SelectContent>
                            </Select>

                            <FormMessage />
                          </FormItem>
                        )}
                      />
                      <div className="grid grid-cols-3 gap-2">
                        <FormField
                          control={form.control}
                          name="card.number"
                          render={({ field }) => (
                            <FormItem className="mb-6 text-start col-span-2">
                              <FormLabel className="text-xs text-start font-thin text-grayColor">
                                Card Number
                              </FormLabel>
                              <FormControl>
                                <Input
                                  type="text"
                                  maxLength={24}
                                  placeholder="Enter Card Number"
                                  {...form.register('card.number', {
                                    pattern: /^(\d{4} ){4}\d{3}$/i,
                                  })}
                                  onChange={(e) =>
                                    formatCardNum(e.target.value)
                                  }
                                  // {...field}
                                />
                              </FormControl>
                              <FormMessage />
                            </FormItem>
                          )}
                        />
                        <FormField
                          control={form.control}
                          name="card.cvv"
                          render={({ field }) => (
                            <FormItem className="mb-6 text-start col-span-1">
                              <FormLabel className="text-xs text-start font-thin text-grayColor">
                                Card CVV
                              </FormLabel>
                              <FormControl>
                                <Input
                                  type="text"
                                  maxLength={3}
                                  placeholder="Enter Cvv"
                                  {...field}
                                />
                              </FormControl>
                              <FormMessage />
                            </FormItem>
                          )}
                        />
                      </div>
                      <div className="grid grid-cols-1 mdx:grid-cols-2 gap-2">
                        <div className=" grid grid-cols-1 mdx:grid-cols-2 gap-2">
                          <FormField
                            control={form.control}
                            name="card.expiryMonth"
                            render={({ field }) => (
                              <FormItem className=" text-start ">
                                <FormLabel className="text-xs text-start font-thin text-grayColor">
                                  Expiry Month
                                </FormLabel>
                                <FormControl>
                                  <Input
                                    type="text"
                                    maxLength={2}
                                    placeholder="Expiry Month"
                                    autoComplete="cc-number"
                                    {...field}
                                  />
                                </FormControl>
                                <FormMessage />
                              </FormItem>
                            )}
                          />
                          <FormField
                            control={form.control}
                            name="card.expiryYear"
                            render={({ field }) => (
                              <FormItem className=" text-start ">
                                <FormLabel className="text-xs text-start font-thin text-grayColor">
                                  Expiry Year
                                </FormLabel>
                                <FormControl>
                                  <Input
                                    type="text"
                                    maxLength={4}
                                    placeholder="Expiry Year"
                                    autoComplete="cc-number"
                                    {...field}
                                  />
                                </FormControl>
                                <FormMessage />
                              </FormItem>
                            )}
                          />
                        </div>
                        <div>
                          <FormField
                            control={form.control}
                            name="card.holder"
                            render={({ field }) => (
                              <FormItem className=" text-start ">
                                <FormLabel className="text-xs text-start font-thin text-grayColor">
                                  Card Holder
                                </FormLabel>
                                <FormControl>
                                  <Input
                                    type="text"
                                    maxLength={38}
                                    placeholder="Card Holder"
                                    autoComplete="cc-number"
                                    {...field}
                                  />
                                </FormControl>
                                <FormMessage />
                              </FormItem>
                            )}
                          />
                        </div>
                      </div>
                    </>
                  )}
                </DialogDescription>
              </DialogHeader>
              <DialogFooter>
                <Button type="submit">Make Payment</Button>
              </DialogFooter>
            </form>
          </Form>
        </DialogContent>
      </Dialog>
      <LoadingDialog open={loading} text={'Saving data...'} />
    </>
  );
}
