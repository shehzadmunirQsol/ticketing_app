import { zodResolver } from '@hookform/resolvers/zod';
import { Button } from '@/ui/button';
import {
  Form,
  FormControl,
  FormField,
  FormItem,
  FormLabel,
  FormMessage,
} from '@/ui/form';
import { Input } from '@/ui/input';
import { useForm } from 'react-hook-form';
import { useEffect, useState } from 'react';
import { getS3ImageUrl } from '~/service/api/s3Url.service';
import { trpc } from '~/utils/trpc';
import { useRouter } from 'next/router';
import { compressImage } from '~/utils/helper';
import { LoadingDialog } from '../modal/loadingModal';
import { LanguageInterface } from '../language_select';
import { createCouponSchema } from '~/schema/coupon';
import {
  Select,
  SelectContent,
  SelectGroup,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from '~/components/ui/select';
import { useToast } from '~/components/ui/use-toast';

export default function CouponForm() {
  const { toast } = useToast();

  const [loading, setLoading] = useState<boolean>(false);
  const [endDate, setEndDate] = useState<any>(
    new Date().toISOString().split('T')[0],
  );

  const router = useRouter();

  // 1. Define your form.
  const form = useForm<createCouponSchema>({
    resolver: zodResolver(createCouponSchema),
    defaultValues: {
      user_id: 1,
      is_percentage: '1',
      is_limited: '0',
    },
  });

  const { index } = router.query;
  const initialOrderFilters: any = {};
  if (index) initialOrderFilters.coupon_id = +index;

  const {
    data: categoryData,
    isLoading,
    isFetched,
  } = trpc.coupon.getById.useQuery(initialOrderFilters, {
    refetchOnWindowFocus: false,
    enabled: index ? true : false,
  });
  useEffect(() => {
    if (!isLoading && isFetched && categoryData !== undefined) {
      form.setValue('name', categoryData?.data?.name as string);
      form.setValue('coupon_code', categoryData?.data?.coupon_code as string);
      form.setValue('discount', categoryData?.data?.discount);
      form.setValue(
        'is_percentage',
        categoryData?.data?.is_percentage ? '1' : '0',
      );
      form.setValue('is_limited', categoryData?.data?.is_limited ? '1' : '0');
      form.setValue('coupon_limit', categoryData?.data?.coupon_limit ?? 0);
      if (categoryData?.data?.start_date) {
        form.setValue('start_date', categoryData?.data?.start_date as any);
      }
      if (categoryData?.data?.end_date) {
        form.setValue('end_date', categoryData?.data?.end_date as any);
      }
    }
  }, [isLoading, isFetched, categoryData, form]);

  const addCoupon = trpc.coupon.create.useMutation();
  const updateCoupon = trpc.coupon.updateCoupon.useMutation();

  // 2. Define a submit handler.
  async function onSubmit(values: createCouponSchema) {
    // Do something with the form values.
    // ✅ This will be type-safe and validated.
    console.log({ values });

    try {
      setLoading(true);
      let data;
      if (index) {
        data = await updateCoupon.mutateAsync({ ...values, coupon_id: +index });
      } else {
        data = await addCoupon.mutateAsync(values);
      }

      if (data) {
        toast({
          variant: 'success',
          title: `Coupon ${index ? 'Updated' : 'Uploaded'} Successfully`,
        });
        setLoading(false);
        router.back();
      } else {
        throw new Error('Data Create Error');
      }
    } catch (error: any) {
      setLoading(false);
      toast({
        variant: 'destructive',
        title: error?.message,
      });
    }
  }

  useEffect(() => {
    try {
      if (form.watch('start_date') !== null) {
        const startDate = new Date(form?.watch('start_date').getTime());
        setEndDate(
          new Date(startDate?.setDate(startDate?.getDate() + 7))
            .toISOString()
            .split('T')[0],
        );
      } else {
        setEndDate(new Date().toISOString().split('T')[0]);
      }
    } catch (e) {
      setEndDate(new Date().toISOString().split('T')[0]);
    }
  }, [form.watch('start_date'), form]);
  console.log(form.watch('start_date'), 'start_date');
  return (
    <Form {...form}>
      <form onSubmit={form.handleSubmit(onSubmit)} className="space-y-4">
        <div className=" grid grid-cols-1 lg:grid-cols-2  gap-4">
          <FormField
            control={form.control}
            name="name"
            render={({ field }) => (
              <FormItem className=" flex flex-col gap-2 mt-2 w-full">
                <FormLabel>
                  Name <sup className="text-md text-red-500">*</sup>
                </FormLabel>
                <FormControl>
                  <Input placeholder="Enter Coupon Name" {...field} />
                </FormControl>
                <FormMessage />
              </FormItem>
            )}
          />
          <FormField
            control={form.control}
            name="coupon_code"
            render={({ field }) => (
              <FormItem className=" flex flex-col gap-2 mt-2 w-full">
                <FormLabel>
                  Coupon Code <sup className="text-md text-red-500">*</sup>
                </FormLabel>
                <FormControl>
                  <Input
                    className="uppercase"
                    placeholder="Enter Coupon Code"
                    {...field}
                  />
                </FormControl>
                <FormMessage />
              </FormItem>
            )}
          />

          <FormField
            control={form.control}
            name="is_percentage"
            render={({ field }) => (
              <FormItem className=" flex flex-col gap-2 mt-2 w-full">
                <FormLabel>
                  Discount Type <sup className="text-md text-red-500">*</sup>
                </FormLabel>
                <FormControl>
                  <Select
                    onValueChange={field.onChange}
                    defaultValue={field.value}
                    value={field.value}
                  >
                    <FormControl>
                      <SelectTrigger className=" rounded-none  ">
                        <SelectValue placeholder={`Select Limit`} />
                      </SelectTrigger>
                    </FormControl>
                    <SelectContent>
                      <SelectGroup>
                        <SelectItem value={'1'}>{'percentage'}</SelectItem>
                        <SelectItem value={'0'}>{'fixed'}</SelectItem>
                      </SelectGroup>
                    </SelectContent>
                  </Select>
                </FormControl>
                <FormMessage />
              </FormItem>
            )}
          />
          <FormField
            control={form.control}
            name="discount"
            render={({ field }) => (
              <FormItem className=" flex flex-col gap-2 mt-2 w-full">
                <FormLabel>
                  Discount Amount in{' '}
                  {form.watch('is_percentage') == '1' ? '%' : 'AED'}{' '}
                  <sup className="text-md text-red-500">*</sup>
                </FormLabel>
                <FormControl>
                  <Input
                    type={'number'}
                    min={1}
                    max={form.watch('is_percentage') == '1' ? 100 : 100000}
                    placeholder={'Enter Discount '}
                    {...form.register('discount', {
                      valueAsNumber: true,
                    })}
                  />
                </FormControl>
                <FormMessage />
              </FormItem>
            )}
          />
          {!index && (
            <FormField
              control={form.control}
              name="start_date"
              render={({ field }) => (
                <FormItem className=" flex flex-col gap-2 mt-2 w-full">
                  <FormLabel>
                    Start Date
                    <sup className="text-md text-red-500">*</sup>
                  </FormLabel>
                  <FormControl>
                    <Input
                      type={'date'}
                      placeholder={'Start Date'}
                      min={new Date().toISOString().split('T')[0]}
                      {...form.register('start_date', {
                        valueAsDate: true,
                      })}
                    />
                  </FormControl>
                  <FormMessage />
                </FormItem>
              )}
            />
          )}
          {!index && (
            <FormField
              control={form.control}
              name="end_date"
              render={({ field }) => (
                <FormItem className=" flex flex-col gap-2 mt-2 w-full">
                  <FormLabel>
                    End Date
                    <sup className="text-md text-red-500">*</sup>
                  </FormLabel>
                  <FormControl>
                    <Input
                      type={'date'}
                      placeholder={'End Date'}
                      min={endDate}
                      {...form.register('end_date', {
                        valueAsDate: true,
                      })}
                    />
                  </FormControl>
                  <FormMessage />
                </FormItem>
              )}
            />
          )}

          <FormField
            control={form.control}
            name={'is_limited'}
            render={({ field }) => (
              <FormItem className=" flex flex-col gap-2 mt-2 w-full">
                <FormLabel>
                  Coupon Type <sup className="text-md text-red-500">*</sup>
                </FormLabel>
                <Select
                  onValueChange={field.onChange}
                  defaultValue={field.value}
                  value={field.value}
                >
                  <FormControl>
                    <SelectTrigger className=" rounded-none">
                      <SelectValue placeholder={`Select Limit`} />
                    </SelectTrigger>
                  </FormControl>
                  <SelectContent>
                    <SelectGroup>
                      <SelectItem value={'1'}>{'limited'}</SelectItem>
                      <SelectItem value={'0'}>{'unlimited'}</SelectItem>
                    </SelectGroup>
                  </SelectContent>
                </Select>

                <FormMessage />
              </FormItem>
            )}
          />
          {form.watch('is_limited') == '1' && (
            <FormField
              control={form.control}
              name="coupon_limit"
              render={({ field }) => (
                <FormItem className=" flex flex-col gap-2 mt-2 w-full">
                  <FormLabel>
                    Coupon Limit <sup className="text-md text-red-500"></sup>
                  </FormLabel>
                  <FormControl>
                    <Input
                      type="number"
                      required={form.watch('is_limited') == '1' ? true : false}
                      placeholder="Enter Coupon Limit"
                      {...form.register('coupon_limit', {
                        valueAsNumber: true,
                      })}
                    />
                  </FormControl>
                  <FormMessage />
                </FormItem>
              )}
            />
          )}
        </div>

        <div className="flex justify-between">
          <div></div>
          <Button type="submit" variant={'clip'}>
            {index ? 'Edit' : 'Add'} Coupon
          </Button>
        </div>
      </form>

      <LoadingDialog
        open={addCoupon.isLoading || loading || (index ? isLoading : false)}
        text={`${index ? 'Updating' : 'Adding'} Coupon...`}
      />
    </Form>
  );
}