import { zodResolver } from '@hookform/resolvers/zod';
import * as z from 'zod';

import { Button } from '@/ui/button';
import {
  Form,
  FormControl,
  FormDescription,
  FormField,
  FormItem,
  FormLabel,
  FormMessage,
} from '@/ui/form';
import { Input } from '@/ui/input';
import { useForm } from 'react-hook-form';
import { useRouter } from 'next/router';
import { Tabs, TabsContent, TabsList, TabsTrigger } from '~/components/ui/tabs';
import { FileInput } from '~/components/common/file_input';
import { useEffect, useState } from 'react';
import { trpc } from '~/utils/trpc';
import { getS3ImageUrl } from '~/service/api/s3Url.service';
import { compressImage, isValidImageType } from '~/utils/helper';
import { useToast } from '~/components/ui/use-toast';
import { LoadingDialog } from '../modal/loadingModal';

const BannerFormSchema = z.object({
  thumb: z.any(),
  link: z.string(),

  en: z.object({
    model: z.string(),
    title: z.string(),
    price: z.string(),
    description: z.string(),
    date: z.string(),
  }),
  ar: z.object({
    model: z.string(),
    title: z.string(),
    price: z.string(),
    description: z.string(),
    date: z.string(),
  }),
});
const enFormSchema = z.object({
  thumb: z.any(),
  link: z.string(),

  en: z.object({
    model: z.string(),
    title: z.string(),
    price: z.string(),
    description: z.string(),
    date: z.string(),
  }),
  ar: z
    .object({
      model: z.string(),
      title: z.string(),
      price: z.string(),
      description: z.string(),
      date: z.string(),
    })
    .optional(),
});
const arFormSchema = z.object({
  thumb: z.any(),
  link: z.string(),
  en: z
    .object({
      model: z.string(),
      title: z.string(),
      price: z.string(),
      description: z.string(),
      date: z.string(),
    })
    .optional(),
  ar: z.object({
    model: z.string(),
    title: z.string(),
    price: z.string(),
    description: z.string(),
    date: z.string(),
  }),
});

export function BannerForm() {
  const { toast } = useToast();

  const router = useRouter();
  const [optimizeFile, setOptimizeFile] = useState<any>(null);
  const [editData, seteditData] = useState<any>(null);
  const [isSubmitting, setIsSubmitting] = useState(false);
  const { index } = router.query;
  const initialOrderFilters: any = {
    rows: 10,
    first: 0,
    page: 0,
  };
  if (index) initialOrderFilters.banner_id = +index;
  const [orderFilters, setOrderFilters] = useState({
    ...initialOrderFilters,
  });
  const {
    data: BannerApiData,
    refetch: BannerRefetch,
    isFetched,
    isLoading,
    isError,
  } = trpc.settings.get_banner.useQuery(orderFilters, {
    refetchOnWindowFocus: false,

    enabled: index ? true : false,
  });
  useEffect(() => {
    if (!isLoading && isFetched && BannerApiData !== undefined) {
      const data: any = { ...BannerApiData[0] };
      seteditData(data);
      const json_data = JSON.parse(data?.value);
      form.setValue('link', json_data?.link);
      form.setValue('thumb', json_data?.thumb);
      if (data?.lang_id == 1) {
        form.setValue('en.model', json_data?.model);
        form.setValue('en.title', json_data?.title);
        form.setValue('en.price', json_data?.price);
        form.setValue('en.description', json_data?.description);
        form.setValue('en.date', json_data?.date);
      } else {
        form.setValue('ar.model', json_data?.model);
        form.setValue('ar.title', json_data?.title);
        form.setValue('ar.price', json_data?.price);
        form.setValue('ar.description', json_data?.description);
        form.setValue('ar.date', json_data?.date);
      }
    }
  }, [isLoading, isFetched]);
  const formValidateData =
    BannerApiData !== undefined && index
      ? BannerApiData[0]?.lang_id
        ? enFormSchema
        : BannerApiData[0]?.lang_id == 2
        ? arFormSchema
        : BannerFormSchema
      : BannerFormSchema;

  const form = useForm<z.infer<typeof formValidateData>>({
    resolver: zodResolver(
      BannerApiData !== undefined && index
        ? BannerApiData[0]?.lang_id == 1
          ? enFormSchema
          : BannerApiData[0]?.lang_id == 2
          ? arFormSchema
          : BannerFormSchema
        : BannerFormSchema,
    ),
  });

  const bannerUpload = trpc.settings.banner_create.useMutation({
    onSuccess: () => {
      console.log('upload successfully');

      // router.push('/store/wallet-connect');
    },
    onError(error: any) {
      console.log({ error });
    },
  });
  const bannerUpdate = trpc.settings.banner_update.useMutation({
    onSuccess: () => {
      console.log('upload successfully');

      // router.push('/store/wallet-connect');
    },
    onError(error: any) {
      console.log({ error });
    },
  });
  // 1. Define your form.

  // 2. Define a submit handler.
  async function onSubmit(values: z.infer<typeof formValidateData>) {
    try {
      setIsSubmitting(true);
      const nftSource =
        typeof form.getValues('thumb') !== 'object'
          ? { thumb: values?.thumb }
          : await uploadOnS3Handler();
      const payload: any = { ...values };
      if (index) {
        let dataPayload: any;
        if (editData?.lang_id == 1) {
          dataPayload = {
            id: +index,
            lang_id: 1,
            key: 'banner_en',
            value: JSON.stringify({
              ...nftSource,
              link: values?.link,
              ...values?.en,
            }),
          };
        } else {
          dataPayload = {
            id: +index,

            lang_id: 2,
            key: 'banner_ar',
            value: JSON.stringify({
              ...nftSource,
              link: values?.link,
              ...values?.ar,
            }),
          };
        }
        const data = await bannerUpdate.mutateAsync({ ...dataPayload });
        if (data) {
          toast({
            variant: 'success',
            title: 'Banner Updated Successfully',
          });
          setIsSubmitting(false);
          router.back();
        } else {
          throw new Error('Data update Error');
        }
      } else {
        const dataPayload = [
          {
            lang_id: 1,
            key: 'banner_en',
            value: JSON.stringify({
              ...nftSource,
              link: values?.link,
              ...values?.en,
            }),
          },
          {
            lang_id: 2,
            key: 'banner_ar',
            value: JSON.stringify({
              ...nftSource,
              link: values?.link,
              ...values?.ar,
            }),
          },
        ];
        const data = await bannerUpload.mutateAsync(dataPayload);
        if (data) {
          toast({
            variant: 'success',
            title: 'Banner Uploaded Successfully',
          });
          setIsSubmitting(false);
          router.back();
        } else {
          throw new Error('Data Create Error');
        }
      }
    } catch (e: any) {
      console.log(e.message, 'e.message');
      setIsSubmitting(false);

      toast({
        variant: 'destructive',
        title: 'Uh oh! Something went wrong.',
        description: 'There was a problem with your request.',
      });
    }
  }
  async function imageHandler(originalFile: File) {
    const optimizedFile = await compressImage(originalFile);
    setOptimizeFile(optimizedFile);
  }

  async function uploadOnS3Handler() {
    if (optimizeFile?.name) {
      const response = await getS3ImageUrl(optimizeFile);
      if (!response.success)
        return console.log('response.message', response.message);

      const isImage = isValidImageType(optimizeFile?.type);

      const nftSource = {
        thumb: '',
      };

      if (isImage) {
        nftSource.thumb = response?.data;
      }

      return nftSource;
    } else {
      return console.log('Please Select Image');
    }
  }
  return (
    <Form {...form}>
      <form
        onSubmit={form.handleSubmit(onSubmit)}
        className="justify-center items-center px-8 py-4 space-y-4"
      >
        <div className="space-y-4">
          <div>
            {!index && (
              <FileInput
                register={form.register('thumb')}
                reset={form.reset}
                getValues={form.getValues}
                setValue={form.setValue}
                imageCompressorHandler={imageHandler}
                required={true}
              />
            )}
            <FormField
              control={form.control}
              name="link"
              render={({ field }) => (
                <FormItem>
                  <FormLabel>Link</FormLabel>
                  <FormControl>
                    <Input type="text" placeholder="Enter LInk" {...field} />
                  </FormControl>
                  <FormMessage />
                </FormItem>
              )}
            />
          </div>
          <div>
            {form.formState.errors?.ar && form.formState.errors?.en && (
              <div className="flex gap-2 items-center p-2  text-destructive bg-white bg-opacity-60 rounded-md">
                <i className="fa-solid fa-circle-info"></i>Please Fill English &
                Arabic form
              </div>
            )}
          </div>
          <div>
            {form.formState.errors?.en && !form.formState.errors?.ar && (
              <div className="flex gap-2 items-center p-2  text-destructive bg-white bg-opacity-60 rounded-md">
                <i className="fa-solid fa-circle-info"></i>
                <>Please Fill English form</>
              </div>
            )}
            {!form.formState.errors?.en && form.formState.errors?.ar && (
              <div className="flex gap-2 items-center p-2  text-destructive bg-white bg-opacity-60 rounded-md">
                <i className="fa-solid fa-circle-info"></i>
                <>Please Fill Arabic form</>
              </div>
            )}
          </div>
          <Tabs
            defaultValue={
              index &&
              BannerApiData !== undefined &&
              BannerApiData[0]?.lang_id == 1
                ? 'en'
                : index &&
                  BannerApiData !== undefined &&
                  BannerApiData[0]?.lang_id == 2
                ? 'ar'
                : 'en'
            }
            className="w-full"
          >
            {index ? (
              <></>
            ) : (
              <>
                <TabsList>
                  <TabsTrigger value="en">English</TabsTrigger>
                  <TabsTrigger value="ar">Arabic</TabsTrigger>
                </TabsList>
              </>
            )}
            <TabsContent value="en">
              <FormField
                control={form.control}
                name="en.model"
                render={({ field }) => (
                  <FormItem>
                    <FormLabel>Modal</FormLabel>
                    <FormControl>
                      <Input type="text" placeholder="Enter Modal" {...field} />
                    </FormControl>
                    <FormMessage />
                  </FormItem>
                )}
              />
              <FormField
                control={form.control}
                name="en.title"
                render={({ field }) => (
                  <FormItem>
                    <FormLabel>Title</FormLabel>
                    <FormControl>
                      <Input type="text" placeholder="Enter Title" {...field} />
                    </FormControl>
                    <FormMessage />
                  </FormItem>
                )}
              />
              <FormField
                control={form.control}
                name="en.price"
                render={({ field }) => (
                  <FormItem>
                    <FormLabel>Price</FormLabel>
                    <FormControl>
                      <Input type="text" placeholder="Enter Price" {...field} />
                    </FormControl>
                    <FormMessage />
                  </FormItem>
                )}
              />
              <FormField
                control={form.control}
                name="en.description"
                render={({ field }) => (
                  <FormItem>
                    <FormLabel>description</FormLabel>
                    <FormControl>
                      <Input
                        type="text"
                        placeholder="Enter description"
                        {...field}
                      />
                    </FormControl>
                    <FormMessage />
                  </FormItem>
                )}
              />
              <FormField
                control={form.control}
                name="en.date"
                render={({ field }) => (
                  <FormItem>
                    <FormLabel>Date</FormLabel>
                    <FormControl>
                      <Input type="text" placeholder="Enter Date" {...field} />
                    </FormControl>
                    <FormMessage />
                  </FormItem>
                )}
              />
            </TabsContent>
            <TabsContent value="ar">
              <div dir="rtl">
                <FormField
                  control={form.control}
                  name="ar.model"
                  render={({ field }) => (
                    <FormItem>
                      <FormLabel>مشروط</FormLabel>
                      <FormControl>
                        <Input
                          type="text"
                          placeholder="أدخل مشروط"
                          {...field}
                        />
                      </FormControl>
                      <FormMessage />
                    </FormItem>
                  )}
                />
                <FormField
                  control={form.control}
                  name="ar.title"
                  render={({ field }) => (
                    <FormItem>
                      <FormLabel>عنوا</FormLabel>
                      <FormControl>
                        <Input
                          type="text"
                          placeholder="أدخل العنوان"
                          {...field}
                        />
                      </FormControl>
                      <FormMessage />
                    </FormItem>
                  )}
                />
                <FormField
                  control={form.control}
                  name="ar.price"
                  render={({ field }) => (
                    <FormItem>
                      <FormLabel>سعر</FormLabel>
                      <FormControl>
                        <Input type="text" placeholder="مَشرُوع" {...field} />
                      </FormControl>
                      <FormMessage />
                    </FormItem>
                  )}
                />
                <FormField
                  control={form.control}
                  name="ar.description"
                  render={({ field }) => (
                    <FormItem>
                      <FormLabel>وصف</FormLabel>
                      <FormControl>
                        <Input
                          type="text"
                          placeholder="أدخل الوصف"
                          {...field}
                        />
                      </FormControl>
                      <FormMessage />
                    </FormItem>
                  )}
                />
                <FormField
                  control={form.control}
                  name="ar.date"
                  render={({ field }) => (
                    <FormItem>
                      <FormLabel>تاريخ</FormLabel>
                      <FormControl>
                        <Input
                          type="text"
                          placeholder="أدخل التاريخ"
                          {...field}
                        />
                      </FormControl>
                      <FormMessage />
                    </FormItem>
                  )}
                />
              </div>
            </TabsContent>
          </Tabs>
        </div>
        <div className="flex items-center justify-between">
          <div></div>
          <Button type="submit" variant={'clip'} className="w-1/2">
            {index ? 'Edit Banner' : 'Add Banner'}
          </Button>
        </div>
      </form>
      <LoadingDialog open={isSubmitting} text={'Saving data...'} />
    </Form>
  );
}
