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
import { Textarea } from '@/ui/textarea';
import { useForm } from 'react-hook-form';
import { ImageInput } from '../file_input';
import { useState } from 'react';
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

interface CategoryFormInterface {
  language: LanguageInterface;
}

export default function CouponForm(props: CategoryFormInterface) {
  const [image, setImage] = useState<File>();
  const [loading, setLoading] = useState<boolean>(false);

  const router = useRouter();

  const { categoryId = 0 } = router.query;

  const { data: category } = trpc.category.getById.useQuery(
    { category_id: +categoryId },
    {
      refetchOnWindowFocus: false,
      enabled: categoryId ? true : false,
      onSuccess(categoryData) {
        // form.setValue('thumb', categoryData?.data?.thumb);
        // form.setValue('en.name', enData?.name as string);
      },
    },
  );

  const addCategory = trpc.category.create.useMutation();
  const updateCategory = trpc.category.update.useMutation();

  // 1. Define your form.
  const form = useForm<createCouponSchema>({
    resolver: zodResolver(createCouponSchema),
    defaultValues: {
      user_id: 1,
      is_percentage: '1',
      start_date: null,
      end_date: null,
    },
  });

  // 2. Define a submit handler.
  async function onSubmit(values: createCouponSchema) {
    // Do something with the form values.
    // ✅ This will be type-safe and validated.
    console.log({ values });

    try {
      setLoading(true);
    } catch (error) {
      setLoading(false);

      console.log(error);
    }
  }

  async function uploadOnS3Handler() {
    console.log({ image });
    const response = await getS3ImageUrl(image);
    if (!response.success) {
      console.log('response.message', response.message);
      return '';
    } else {
      return response.data;
    }
  }

  async function imageHandler(originalFile: File) {
    const optimizedFile = await compressImage(originalFile);
    setImage(optimizedFile);
  }
  console.log(
    form.watch('start_date') !== null ? new Date(form.watch('start_date').getDate() + 1) : '',
    "form.watch('start_date')",
  );

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
                    min={
                      form.watch('start_date') !== null
                        ? form.watch('start_date').toISOString().split('T')[0]
                        : new Date().toISOString().split('T')[0]
                    }
                    {...form.register('end_date', {
                      valueAsDate: true,
                    })}
                  />
                </FormControl>
                <FormMessage />
              </FormItem>
            )}
          />

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
              name="limit"
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
                      {...field}
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
            Submit
          </Button>
        </div>
      </form>

      <LoadingDialog
        open={addCategory.isLoading || loading}
        text={`${categoryId ? 'Updating' : 'Adding'} Category...`}
      />
    </Form>
  );
}
