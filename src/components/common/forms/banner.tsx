import { zodResolver } from '@hookform/resolvers/zod';
import * as z from 'zod';

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
import { useRouter } from 'next/router';
import { Tabs, TabsContent, TabsList, TabsTrigger } from '~/components/ui/tabs';
import { FileInput } from '~/components/common/file_input';
import { useState } from 'react';
import { trpc } from '~/utils/trpc';
import { getS3ImageUrl } from '~/service/api/s3Url.service';
import { compressImage, isValidImageType } from '~/utils/helper';
import { useToast } from '~/components/ui/use-toast';
import { LoadingDialog } from '../modal/loadingModal';

const BannerFormSchema = z.object({
  thumb: z.any(),
  link: z
    .string({
      required_error: 'Please enter a product link',
    })
    .min(1, {
      message: 'Please enter a product link',
    }),
  en: z.object({
    model: z
      .string({
        required_error: 'Please enter a model name',
      })
      .min(1, {
        message: 'Please enter a model name',
      })
      .trim(),
    title: z
      .string({
        required_error: 'Please enter a title',
      })
      .min(1, {
        message: 'Please enter a title',
      })
      .trim(),
    price: z
      .string({
        required_error: 'Please enter a price',
      })
      .min(1, {
        message: 'Please enter a price',
      })
      .trim(),
    description: z
      .string({
        required_error: 'Please enter a description',
      })
      .min(1, {
        message: 'Please enter a description',
      })
      .trim(),
    date: z
      .string({
        required_error: 'Please enter a date',
      })
      .min(1, {
        message: 'Please enter a date',
      })
      .trim(),
  }),
  ar: z.object({
    model: z
      .string({
        required_error: 'Please enter a model name',
      })
      .min(1, {
        message: 'Please enter a model name',
      })
      .trim(),
    title: z
      .string({
        required_error: 'Please enter a title',
      })
      .min(1, {
        message: 'Please enter a title',
      })
      .trim(),
    price: z
      .string({
        required_error: 'Please enter a price',
      })
      .min(1, {
        message: 'Please enter a price',
      })
      .trim(),
    description: z
      .string({
        required_error: 'Please enter a description',
      })
      .min(1, {
        message: 'Please enter a description',
      })
      .trim(),
    date: z
      .string({
        required_error: 'Please enter a date',
      })
      .min(1, {
        message: 'Please enter a date',
      })
      .trim(),
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
  const {
    data: BannerApiData,
    isFetched,
    isLoading,
    isFetching,
  } = trpc.settings.get_banner.useQuery(initialOrderFilters, {
    refetchOnWindowFocus: false,
    onSuccess(bannerData) {
      const enData: any = bannerData?.data.find((cat) => cat.lang_id === 1);
      const arData: any = bannerData?.data.find((cat) => cat.lang_id !== 1);
      form.setValue('link', enData?.link as string);
      form.setValue('thumb', enData?.thumb);
      // en
      form.setValue('en.model', enData?.model);
      form.setValue('en.title', enData?.title);
      form.setValue('en.price', enData?.price);
      form.setValue('en.description', enData?.description);
      form.setValue('en.date', enData?.date);
      // ar
      form.setValue('ar.model', arData?.model);
      form.setValue('ar.title', arData?.title);
      form.setValue('ar.price', arData?.price);
      form.setValue('ar.description', arData?.description);
      form.setValue('ar.date', arData?.date);
    },

    enabled: initialOrderFilters.banner_id !== undefined ? true : false,
  });
  const formValidateData = BannerFormSchema;

  const form = useForm<z.infer<typeof formValidateData>>({
    resolver: zodResolver(BannerFormSchema),
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
      if (index) {
        const dataPayload = {
          id: +index,

          key: 'banner_en',
          value: JSON.stringify({
            ...nftSource,
            link: values?.link,
            ...values?.en,
          }),
          en: {
            lang_id: 1,
            key: 'banner_en',
            value: JSON.stringify({
              ...nftSource,
              link: values?.link,
              ...values?.en,
            }),
          },
          ar: {
            lang_id: 2,
            key: 'banner_ar',
            value: JSON.stringify({
              ...nftSource,
              link: values?.link,
              ...values?.ar,
            }),
          },
        };
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
        const dataPayload = {
          key: 'banner_en',
          value: JSON.stringify({
            ...nftSource,
            link: values?.link,
            ...values?.en,
          }),
          en: {
            lang_id: 1,
            key: 'banner_en',
            value: JSON.stringify({
              ...nftSource,
              link: values?.link,
              ...values?.en,
            }),
          },
          ar: {
            lang_id: 2,
            key: 'banner_ar',
            value: JSON.stringify({
              ...nftSource,
              link: values?.link,
              ...values?.ar,
            }),
          },
        };

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
        title: e?.message,
      });
    }
  }
  async function imageHandler(originalFile: File) {
    const optimizedFile = await compressImage(originalFile);
    setOptimizeFile(optimizedFile);
  }

  async function uploadOnS3Handler() {
    console.log('uploading');
    if (optimizeFile?.name) {
      const response = await getS3ImageUrl(optimizeFile);
      if (!response.success) throw new Error('Image Upload Failure');

      const isImage = isValidImageType(optimizeFile?.type);

      const nftSource = {
        thumb: '',
      };

      if (isImage) {
        nftSource.thumb = response?.data;
      }

      return nftSource;
    } else {
      throw new Error('Please Select Image');
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
            <FileInput
              register={form.register('thumb')}
              reset={form.reset}
              getValues={form.getValues}
              setValue={form.setValue}
              imageCompressorHandler={imageHandler}
              required={true}
            />

            <FormField
              control={form.control}
              name="link"
              render={({ field }) => (
                <FormItem>
                  <FormLabel>Link</FormLabel>
                  <FormControl>
                    <div className="flex bg-input items-center">
                      <p className="border border-input px-4">
                        {process.env.NEXT_PUBLIC_BASE_URL}
                      </p>
                      <Input
                        type="text flex-1"
                        placeholder="Enter Link"
                        {...field}
                      />
                    </div>
                  </FormControl>
                  <div className="relative pb-4 errormsg">
                    <FormMessage />
                  </div>
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
          <Tabs defaultValue={'en'} className="w-full">
            <TabsList className="overflow-hidden">
              <TabsTrigger value="en">English</TabsTrigger>
              <TabsTrigger value="ar">Arabic</TabsTrigger>
            </TabsList>

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
                    <div className="relative pb-4">
                      <FormMessage />
                    </div>
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
                    <div className="relative pb-4">
                      <FormMessage />
                    </div>
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
                    <div className="relative pb-4">
                      <FormMessage />
                    </div>
                  </FormItem>
                )}
              />
              <FormField
                control={form.control}
                name="en.description"
                render={({ field }) => (
                  <FormItem>
                    <FormLabel>Description</FormLabel>
                    <FormControl>
                      <Input
                        type="text"
                        placeholder="Enter description"
                        {...field}
                      />
                    </FormControl>

                    <div className="relative pb-4">
                      <FormMessage />
                    </div>
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

                    <div className="relative pb-4">
                      <FormMessage />
                    </div>
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

                      <div className="relative pb-4">
                        <FormMessage />
                      </div>
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

                      <div className="relative pb-4">
                        <FormMessage />
                      </div>
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

                      <div className="relative pb-4">
                        <FormMessage />
                      </div>
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

                      <div className="relative pb-4">
                        <FormMessage />
                      </div>
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

                      <div className="relative pb-4">
                        <FormMessage />
                      </div>
                    </FormItem>
                  )}
                />
              </div>
            </TabsContent>
          </Tabs>
        </div>
        <div className="flex items-center justify-end">
          <Button type="submit" variant={'clip'} className="w-1/2">
            {index ? 'Edit Banner' : 'Add Banner'}
          </Button>
        </div>
      </form>
      <LoadingDialog
        open={isSubmitting || (index ? isLoading : false)}
        text={`${isSubmitting ? 'Saving' : 'Loading'} data...`}
      />
    </Form>
  );
}
