import { zodResolver } from '@hookform/resolvers/zod';
import { useForm } from 'react-hook-form';
import { Button } from '~/components/ui/button';
import {
  Dialog,
  DialogContent,
  DialogDescription,
  DialogHeader,
  DialogTitle,
} from '~/components/ui/dialog';
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
import { useToast } from '~/components/ui/use-toast';
import { addAddressInput, addCustomerAddress } from '~/schema/customer';

import { ScrollArea, ScrollBar } from '~/components/ui/scroll-area';
import { trpc } from '~/utils/trpc';
import countryJSON from '~/data/countries.json';
import { LoadingDialog } from './loadingModal';
import { formatTrpcError } from '~/utils/helper';
import { useEffect } from 'react';

import { PhoneInput } from 'react-international-phone';
import 'react-international-phone/style.css';

const countries = countryJSON.map((item) => item.country);

interface SelectCustomerInterface {
  isModal: boolean;
  openChangeHandler: () => void;
  availableAddressTypes?: { label: string; value: string }[];
  refetch: () => void;
  customer_id: number;
  address_type?: 'home' | 'work' | 'hotel' | 'other';
  id?: number;
  street_address_1?: string | null;
  street_address_2?: string | null;
  country?: string | null;
  state?: string | null;
  city?: string | null;
  phone_number?: string | null;
  phone_code?: string | null;
  postal_code?: string | null;
}

export function AddCustomerAddressDialog(props: SelectCustomerInterface) {
  const { toast } = useToast();

  useEffect(() => {
    form.setValue('address_type', props.address_type as any);
    form.setValue('street_address_1', props.street_address_1 ?? '');
    if (props.country) {
      form.setValue('country', props.country);
    }
    form.setValue('city', props.city ?? '');
    form.setValue('phone_number', props.phone_number ?? '');
    form.setValue('phone_code', props.phone_code ?? '');
    form.setValue('postal_code', props.postal_code ?? '');
  }, [props.isModal]);

  const addAddress = trpc.customer.addAddress.useMutation({
    onSuccess: (res) => {
      console.log(res);
      toast({
        variant: 'success',
        title: 'Address added successfully',
      });
    },
  });

  const updateAddress = trpc.customer.updateAddress.useMutation({
    onSuccess: (res) => {
      console.log(res);
      toast({
        variant: 'success',
        title: 'Address Updated Successfully',
      });
    },
    onError(error) {
      const errorMessage = formatTrpcError(error?.shape?.message);

      toast({
        variant: 'destructive',
        title: errorMessage,
      });
    },
  });

  const form = useForm<addAddressInput>({
    resolver: zodResolver(addCustomerAddress),
    defaultValues: {
      customer_id: props.customer_id,
      address_type: props.address_type,
      street_address_1: props.street_address_1 ?? '',
      city: props.city ?? '',
      country: props.country ?? '',
      phone_number: props.phone_number ?? '',
      phone_code: props.phone_code ?? '',
      street_address_2: props.street_address_2 ?? '',
      state: props.state ?? '',
    },
  });

  async function onSubmit(values: addAddressInput) {
    try {
      const payload = { ...values, customer_id: props?.customer_id };
      if (props?.id) {
        await updateAddress.mutateAsync({ ...payload, id: props?.id });
      } else {
        await addAddress.mutateAsync(payload);
      }

      toast({
        variant: 'success',
        title: `Address ${props?.id ? 'Update' : 'Add'} successfully!`,
      });
      props?.refetch();
      props?.openChangeHandler();
    } catch (error: any) {
      props?.openChangeHandler();
      toast({
        variant: 'destructive',
        title: error?.message ?? 'Something went wrong!',
      });
    }
  }

  console.log({ props });

  return (
    <Dialog open={props?.isModal} onOpenChange={props.openChangeHandler}>
      <DialogContent className="p-0">
        <ScrollArea className="w-full max-w-[700px] h-[calc(100vh-50px)] max-h-[440px] md:max-h-[530px] scroll-hide">
          <ScrollBar orientation="vertical"></ScrollBar>
          <DialogHeader className="p-6">
            <DialogTitle>{props?.id ? 'Update' : 'Add'} Address</DialogTitle>
            <DialogDescription>
              <Form {...form}>
                <form
                  onSubmit={form.handleSubmit(onSubmit)}
                  className="justify-center items-center  py-4"
                >
                  <FormField
                    control={form.control}
                    name="address_type"
                    render={({ field }) => (
                      <FormItem className="w-full ">
                        <FormLabel className="text-sm text-cardGray">
                          Address Type <sup className="text-red-500">*</sup>
                        </FormLabel>
                        <Select
                          disabled={props?.id ? true : false}
                          onValueChange={field.onChange}
                          defaultValue={field.value}
                          value={field.value}
                        >
                          <FormControl className="bg-inputColor">
                            <SelectTrigger className=" rounded-md h-10  ">
                              <SelectValue
                                placeholder="Select Billing Address"
                                className=""
                              />
                            </SelectTrigger>
                          </FormControl>

                          <SelectContent className="max-h-[300px] overflow-y-auto">
                            <SelectGroup>
                              {props.availableAddressTypes?.map((address) => {
                                return (
                                  <SelectItem
                                    key={address.value}
                                    value={address.value}
                                  >
                                    {address.label}
                                  </SelectItem>
                                );
                              })}
                            </SelectGroup>
                          </SelectContent>
                        </Select>

                        <div className="relative pb-4">
                          <FormMessage />
                        </div>
                      </FormItem>
                    )}
                  />

                  <FormField
                    control={form.control}
                    name="postal_code"
                    render={() => (
                      <FormItem className=" w-full mb-2">
                        <FormLabel className="text-sm text-cardGray ">
                        P.O. Box 
                        </FormLabel>
                        <FormControl>
                          <Input
                            type="text"
                            className='rounded-md'
                            {...form.register('postal_code')}
                          />
                        </FormControl>
                        <div className="relative pb-6">
                          <FormMessage />
                        </div>
                      </FormItem>
                    )}
                  />
                  <div className="w-full">
                    <FormLabel className="text-sm text-cardGray ">
                      Phone Number <sup className="text-red-500">*</sup>
                    </FormLabel>

                    <div className="flex gap-2">

                    <FormField
                        control={form.control}
                        name="phone_code"
                        render={({ field }) => (
                          <FormItem>
                            <FormControl className="rounded-md ">

                            <PhoneInput
                                className="rounded-md w-20 countrycode"
                                defaultCountry="ae"
                                inputProps={{ minLength: 1, maxLength: 4, placeholder:"+971", readOnly: true, ...field }} 
                                {...field} 
                              /> 

                              {/* <Input
                                minLength={1}
                                type="text"
                                maxLength={4}
                                className="w-20"
                                placeholder="+971"
                                {...field}
                              /> */}
                            </FormControl>

                            <div className="relative pb-5">
                              <FormMessage />
                            </div>
                          </FormItem>
                        )}
                      />
 

                      {/* <FormField
                        control={form.control}
                        name="phone_code"
                        render={({ field }) => (
                          <FormItem>
                            <FormControl className="rounded-md ">
                              <Input
                                minLength={1}
                                type="text"
                                maxLength={4}
                                className="w-20"
                                placeholder="+971"
                                {...field}
                              />
                            </FormControl>

                            <div className="relative pb-5">
                              <FormMessage />
                            </div>
                          </FormItem>
                        )}
                      /> */}
                      <FormField
                        control={form.control}
                        name="phone_number"
                        render={({ field }) => (
                          <FormItem className=" w-full">
                            <FormControl className="rounded-md ">
                              <Input
                                maxLength={15}
                                type="string"
                                className="w-full"
                                {...field}
                              />
                            </FormControl>

                            <div className="relative pb-5">
                              <FormMessage />
                            </div>
                          </FormItem>
                        )}
                      />
                    </div>
                  </div>

                  <FormField
                    control={form.control}
                    name="country"
                    render={({ field }) => (
                      <FormItem className="w-full ">
                        <FormLabel className="text-sm text-cardGray">
                          Country / Region <sup className="text-red-500">*</sup>
                        </FormLabel>
                        <Select
                          onValueChange={field.onChange}
                          value={field.value}
                        >
                          <FormControl className="bg-inputColor">
                            <SelectTrigger className=" rounded-md h-10  ">
                              <SelectValue
                                className=""
                              />
                            </SelectTrigger>
                          </FormControl>

                          <SelectContent className="max-h-[300px] overflow-y-auto">
                            <SelectGroup>
                              {countries?.map((country) => {
                                return (
                                  <SelectItem key={country} value={country}>
                                    {country?.toUpperCase()}
                                  </SelectItem>
                                );
                              })}
                            </SelectGroup>
                          </SelectContent>
                        </Select>

                        <div className="relative pb-5">
                          <FormMessage />
                        </div>
                      </FormItem>
                    )}
                  />
                  <FormField
                    control={form.control}
                    name="state"
                    render={({ field }) => (
                      <FormItem className=" w-full ">
                        <FormLabel className="text-sm text-cardGray">
                          State <sup className="text-red-500">*</sup>
                        </FormLabel>
                        <FormControl>
                          <Input
                            type="text"
                            className='rounded-md'
                            {...field}
                          />
                        </FormControl>
                        <div className="relative pb-5">
                          <FormMessage />
                        </div>
                      </FormItem>
                    )}
                  />
                  <FormField
                    control={form.control}
                    name="city"
                    render={({ field }) => (
                      <FormItem className=" w-full ">
                        <FormLabel className="text-sm text-cardGray">
                          City <sup className="text-red-500">*</sup>
                        </FormLabel>
                        <FormControl>
                          <Input
                            type="text"
                            className='rounded-md'
                            {...field}
                          />
                        </FormControl>
                        <div className="relative pb-5">
                          <FormMessage />
                        </div>
                      </FormItem>
                    )}
                  />
                  <FormField
                    control={form.control}
                    name="street_address_1"
                    render={({ field }) => (
                      <FormItem className=" w-full ">
                        <FormLabel className="text-sm text-cardGray">
                          Street Address <sup className="text-red-500">*</sup>
                        </FormLabel>
                        <FormControl>
                          <Input
                            type="text"
                            className='rounded-md'
                            {...field}
                          />
                        </FormControl>
                        <div className="relative pb-5">
                          <FormMessage />
                        </div>
                      </FormItem>
                    )}
                  />
                  <FormField
                    control={form.control}
                    name="street_address_2"
                    render={({ field }) => (
                      <FormItem className=" w-full ">
                        <FormLabel className="text-sm text-cardGray">
                        Apartment No.
                        </FormLabel>
                        <FormControl>
                          <Input
                            type="text"
                            className='rounded-md'
                            {...field}
                          />
                        </FormControl>
                        <div className="relative pb-2">
                          <FormMessage />
                        </div>
                      </FormItem>
                    )}
                  />
                  <div className="flex items-center justify-end gap-4">
                    <Button
                      variant="secondary"
                      type="button"
                      disabled={addAddress.isLoading || updateAddress.isLoading}
                      onClick={props.openChangeHandler}
                    >
                      Cancel
                    </Button>
                    <Button
                      type="submit"
                      disabled={addAddress.isLoading || updateAddress.isLoading}
                    >
                      {props?.id ? 'Update' : 'Add'}
                    </Button>
                  </div>
                </form>
              </Form>
            </DialogDescription>
          </DialogHeader>
        </ScrollArea>
      </DialogContent>
      <LoadingDialog
        open={addAddress.isLoading || updateAddress.isLoading}
        text={`${props?.id ? 'Updating' : 'Adding'} address...`}
      />
    </Dialog>
  );
}
