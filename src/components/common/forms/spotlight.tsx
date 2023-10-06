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
import { useEffect, useState } from 'react';
import { trpc } from '~/utils/trpc';
import { getS3ImageUrl } from '~/service/api/s3Url.service';
import { compressImage, isValidImageType } from '~/utils/helper';
import { useToast } from '~/components/ui/use-toast';
import { Textarea } from '~/components/ui/textarea';
import { LoadingDialog } from '../modal/loadingModal';

const SpotLightFormSchema = z.object({
  thumb: z.any(),
  link: z
    .string({
      required_error: 'Please add link',
    })
    .min(1, { message: 'Please add link' }),

  en: z.object({
    name: z
      .string({
        required_error: 'Please add name',
      })
      .min(1, { message: 'Please add name' })
      .trim(),
    description: z
      .string({
        required_error: 'Please add description',
      })
      .min(1, { message: 'Please add description' })
      .trim(),
  }),
  ar: z.object({
    name: z
      .string({
        required_error: 'Please add name',
      })
      .min(1, { message: 'Please add name' })
      .trim(),
    description: z
      .string({
        required_error: 'Please add description',
      })
      .min(1, { message: 'Please add description' })
      .trim(),
  }),
});

export function SpotLightForm() {
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
    group: 'WONDER',
  };
  if (index) initialOrderFilters.banner_id = +index;
  const [orderFilters, setOrderFilters] = useState({
    ...initialOrderFilters,
  });
  const {
    data: BannerApiData,
    isFetched,
    isLoading,
    isFetching,
  } = trpc.settings.get_banner.useQuery(orderFilters, {
    refetchOnWindowFocus: false,
    onSuccess(spotLightData) {
      const enData: any = spotLightData?.data.find((cat) => cat.lang_id === 1);
      const arData: any = spotLightData?.data.find((cat) => cat.lang_id !== 1);
      console.log({ enData });
      form.setValue('link', enData?.link as string);
      form.setValue('thumb', enData?.thumb);
      // en
      form.setValue('en.name', enData?.name);

      form.setValue('en.description', enData?.description);
      // ar
      form.setValue('ar.name', arData?.name);

      form.setValue('ar.description', arData?.description);
    },
    enabled: index ? true : false,
  });

  const formValidateData = SpotLightFormSchema;

  const form = useForm<z.infer<typeof formValidateData>>({
    resolver: zodResolver(SpotLightFormSchema),
  });

  const spotLightUpload = trpc.settings.banner_create.useMutation({
    onSuccess: () => {
      console.log('upload successfully');

      // router.push('/store/wallet-connect');
    },
    onError(error: any) {
      console.log({ error });
    },
  });
  const spotLightUpdate = trpc.settings.banner_update.useMutation({
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
        const dataPayload = {
          id: +index,

          lang_id: 1,

          group: 'WONDER',
          key: 'spotlight',
          value: JSON.stringify({
            ...nftSource,
            link: values?.link,
            ...values?.en,
          }),
          en: {
            lang_id: 1,

            group: 'WONDER',
            key: 'spotlight',
            value: JSON.stringify({
              ...nftSource,
              link: values?.link,
              ...values?.en,
            }),
          },
          ar: {
            lang_id: 2,

            group: 'WONDER',
            key: 'spotlight',
            value: JSON.stringify({
              ...nftSource,
              link: values?.link,
              ...values?.ar,
            }),
          },
        };
        const data = await spotLightUpdate.mutateAsync({ ...dataPayload });
        if (data) {
          setIsSubmitting(false);

          toast({
            variant: 'success',
            title: 'SpotLight Updated Successfully',
          });
          router.back();
        } else {
          throw new Error('Data update Error');
        }
      } else {
        const dataPayload = {
          key: 'spotlight',
          group: 'WONDER',

          value: JSON.stringify({
            ...nftSource,
            link: values?.link,
            ...values?.en,
          }),
          en: {
            lang_id: 1,
            group: 'WONDER',
            key: 'spotlight',
            value: JSON.stringify({
              ...nftSource,
              link: values?.link,
              ...values?.en,
            }),
          },
          ar: {
            lang_id: 2,
            group: 'WONDER',
            key: 'spotlight',
            value: JSON.stringify({
              ...nftSource,
              link: values?.link,
              ...values?.ar,
            }),
          },
        };

        const data = await spotLightUpload.mutateAsync(dataPayload);
        if (data) {
          setIsSubmitting(false);

          toast({
            variant: 'success',
            title: 'SpotLight Uploaded Successfully',
          });
          router.back();
        } else {
          throw new Error('Data Create Error');
        }
      }
    } catch (e: any) {
      setIsSubmitting(false);

      toast({
        variant: 'destructive',
        title: e.message,
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
  console.log(form.formState.errors, 'form.error');
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
                    <Input type="text" placeholder="Enter Link" {...field} />
                  </FormControl>

                  <div className="relative pb-2">
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
                name="en.name"
                render={({ field }) => (
                  <FormItem>
                    <FormLabel>Name</FormLabel>
                    <FormControl>
                      <Input type="text" placeholder="Enter Name" {...field} />
                    </FormControl>

                    <div className="relative pb-4">
                      <FormMessage />
                    </div>
                  </FormItem>
                )}
              />
              <div>
                <FormField
                  control={form.control}
                  name="en.description"
                  render={({ field }) => (
                    <FormItem>
                      <FormLabel>Description</FormLabel>
                      <FormControl>
                        <Textarea
                          placeholder="Enter Description..."
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
            <TabsContent value="ar">
              <div dir="rtl">
                <FormField
                  control={form.control}
                  name="ar.name"
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
                  name="ar.description"
                  render={({ field }) => (
                    <FormItem>
                      <FormLabel>عنوا</FormLabel>
                      <FormControl>
                        <Textarea placeholder="أدخل العنوان" {...field} />
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
        <div className="flex items-center justify-between">
          <div></div>
          <Button type="submit" variant={'clip'} className="w-1/2">
            {index ? 'Edit Winnar Wonders' : 'Add Winnar Wonders'}
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
