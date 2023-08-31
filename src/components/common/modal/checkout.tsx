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
import { createPaymentSchema } from '~/schema/payment';
import { zodResolver } from '@hookform/resolvers/zod';
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
  const { toast } = useToast();
  const [loading, setLoading] = useState<boolean>(false);
  const form = useForm<createPaymentSchema>({
    resolver: zodResolver(createPaymentSchema),
  });

  const bannerUpdate = trpc.payment.createPayment.useMutation({
    onSuccess: () => {
      console.log('upload successfully');

      // router.push('/store/wallet-connect');
    },
    onError(error: any) {
      console.log({ error });
    },
  });
  const onSubmit = async (values: createPaymentSchema) => {
    console.log(values, 'onSubmit');
    const data = await bannerUpdate.mutateAsync({ ...values, price: 90 });
    console.log({ data });
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
  console.log(form.formState.errors, "form.watch('card_num')");
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
                  <FormField
                    control={form.control}
                    name="paymentBrand"
                    render={({ field }) => (
                      <FormItem>
                        <FormLabel className="text-xs font-thin text-grayColor">
                          Country/ Region
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
                              <SelectItem value={'Master'}>Master</SelectItem>
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
                              onChange={(e) => formatCardNum(e.target.value)}
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
                </DialogDescription>
              </DialogHeader>
              <div className=" py-2"></div>
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
