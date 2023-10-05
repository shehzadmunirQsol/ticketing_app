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
import { trpc } from '~/utils/trpc';
import countryJSON from '~/data/countries.json';
import { LoadingDialog } from './loadingModal';
import { formatTrpcError } from '~/utils/helper';
import { useEffect } from 'react';
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
  postal_code?: number | null;
}

export function AddCustomerAddressDialog(props: SelectCustomerInterface) {
  const { toast } = useToast();

  useEffect(() => {
    form.setValue('address_type', props.address_type as any);
    form.setValue('street_address_1', props.street_address_1 ?? '');
    form.setValue('country', props.country ?? '');
    form.setValue('city', props.city ?? '');
    form.setValue('phone_number', props.phone_number ?? '');
    form.setValue('phone_code', props.phone_code ?? '');
    form.setValue('postal_code', props.postal_code ?? 0);
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
        title: 'Winner Selected successfully!',
      });
      props?.openChangeHandler();
      props?.refetch();
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
      <DialogContent className="max-h-[650px] h-[calc(100%-100px)]  overflow-y-scroll ">
        <DialogHeader>
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

                      <div className="relative pb-2">
                        <FormMessage />
                      </div>
                    </FormItem>
                  )}
                />

                <FormField
                  control={form.control}
                  name="postal_code"
                  render={() => (
                    <FormItem className=" w-full ">
                      <FormLabel className="text-sm text-cardGray ">
                        Postal Code <sup className="text-red-500">*</sup>
                      </FormLabel>
                      <FormControl>
                        <Input
                          type="number"
                          placeholder="Enter postal code"
                          {...form.register('postal_code', {
                            valueAsNumber: true,
                          })}
                        />
                      </FormControl>
                      <div className="relative pb-2">
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
                            <Input
                              min={0}
                              type="number"
                              className="w-14"
                              placeholder="+971"
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
                      name="phone_number"
                      render={({ field }) => (
                        <FormItem className=" w-full">
                          <FormControl className="rounded-md ">
                            <Input
                              min={0}
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

                <FormField
                  control={form.control}
                  name="country"
                  render={({ field }) => (
                    <FormItem className="w-full ">
                      <FormLabel className="text-sm text-cardGray">
                        Country/ Region <sup className="text-red-500">*</sup>
                      </FormLabel>
                      <Select
                        onValueChange={field.onChange}
                        defaultValue={field.value}
                        value={field.value}
                      >
                        <FormControl className="bg-inputColor">
                          <SelectTrigger className=" rounded-md h-10  ">
                            <SelectValue
                              placeholder="Select your country"
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

                      <div className="relative pb-2">
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
                          placeholder="Enter You City"
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
                  name="street_address_1"
                  render={({ field }) => (
                    <FormItem className=" w-full ">
                      <FormLabel className="text-sm text-cardGray">
                        Street Address <sup className="text-red-500">*</sup>
                      </FormLabel>
                      <FormControl>
                        <Input
                          type="text"
                          placeholder="Enter Street Address"
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
                    disabled={addAddress.isLoading}
                    onClick={props.openChangeHandler}
                  >
                    Cancel
                  </Button>
                  <Button type="submit" disabled={addAddress.isLoading}>
                    {props?.id ? 'Update' : 'Add'}
                  </Button>
                </div>
              </form>
            </Form>
          </DialogDescription>
        </DialogHeader>
      </DialogContent>
      <LoadingDialog open={addAddress.isLoading} text="Adding address..." />
    </Dialog>
  );
}
