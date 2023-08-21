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
import { isValidImageType } from '~/utils/helper';
import { useToast } from '~/components/ui/use-toast';
import { Textarea } from '~/components/ui/textarea';

const SpotLightFormSchema = z.object({
  thumb: z.any(),
  link: z.string(),

  en: z.object({
    name: z.string(),
    description: z.string().optional(),
  }),
  ar: z.object({
    name: z.string(),
    description: z.string().optional(),
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
export function SpotLightForm() {
  const { toast } = useToast();

  const router = useRouter();
  const [optimizeFile, setOptimizeFile] = useState<any>(null);
  const [editData, seteditData] = useState<any>(null);
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
      form.setValue('name', json_data?.name);
      form.setValue('description', json_data?.description);
      form.setValue('thumb', json_data?.thumb);
    }
  }, [isLoading, isFetched]);
  const formValidateData =
    BannerApiData !== undefined && index
      ? BannerApiData[0]?.lang_id
        ? enFormSchema
        : BannerApiData[0]?.lang_id == 2
        ? arFormSchema
        : SpotLightFormSchema
      : SpotLightFormSchema;

  const form = useForm<z.infer<typeof formValidateData>>({
    resolver: zodResolver(
      BannerApiData !== undefined && index
        ? BannerApiData[0]?.lang_id
          ? enFormSchema
          : BannerApiData[0]?.lang_id == 2
          ? arFormSchema
          : SpotLightFormSchema
        : SpotLightFormSchema,
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
      const nftSource =
        typeof form.getValues('thumb') !== 'object'
          ? { thumb: values?.thumb }
          : await uploadOnS3Handler();
      const payload: any = { ...values };
      if (index) {
        const dataPayload: any = {
          id: +index,

          group: 'WONDER',
          key: 'spotlight',
          value: JSON.stringify({
            ...nftSource,
            link: values?.link,
            name: values?.name,
            description: values?.description,
          }),
        };

        const data = await bannerUpdate.mutateAsync({ ...dataPayload });
        if (data) {
          toast({
            variant: 'success',
            title: 'Banner Updated Successfully',
          });
          router.back();
        } else {
          throw new Error('Data update Error');
        }
      } else {
        const dataPayload = [
          {
            lang_id: 1,
            group: 'WONDER',
            key: 'spotlight',
            value: JSON.stringify({
              ...nftSource,
              link: values?.link,
              ...values?.en,
            }),
          },
          {
            lang_id: 2,
            group: 'WONDER',
            key: 'spotlight',
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
          router.back();
        } else {
          throw new Error('Data Create Error');
        }
      }
    } catch (e: any) {
      console.log(e.message, 'e.message');
      toast({
        variant: 'destructive',
        title: 'Uh oh! Something went wrong.',
        description: 'There was a problem with your request.',
      });
    }
  }
  async function imageCompressorHandler(originalFile: any) {
    const imageFile = originalFile;
    const imageFilename = originalFile.name;

    if (!imageFile) return 'Please select image.';
    // if (!imageFile.name.match(/\.(jpg|jpeg|png|JPG|JPEG|PNG|gif)$/))
    //   return "Please select valid image JPG,JPEG,PNG";

    const reader = new FileReader();
    reader.readAsDataURL(imageFile);

    reader.onload = (e) => {
      const img = new Image();
      img.onload = () => {
        //------------- Resize img code ----------------------------------
        const canvas = document.createElement('canvas');
        let ctx = canvas.getContext('2d');
        ctx?.drawImage(img, 0, 0);

        const MAX_WIDTH = 437;
        const MAX_HEIGHT = 437;
        let width = img.width;
        let height = img.height;

        if (width > height) {
          if (width > MAX_WIDTH) {
            height *= MAX_WIDTH / width;
            width = MAX_WIDTH;
          }
        } else {
          if (height > MAX_HEIGHT) {
            width *= MAX_HEIGHT / height;
            height = MAX_HEIGHT;
          }
        }
        canvas.width = width;
        canvas.height = height;
        ctx = canvas.getContext('2d');
        ctx?.drawImage(img, 0, 0, width, height);
        ctx?.canvas?.toBlob(
          (blob) => {
            if (blob) {
              const optimizeFile = new File([blob], imageFilename, {
                type: 'image/png',
                lastModified: Date.now(),
              });
              // setFile(originalFile);
              setOptimizeFile(optimizeFile);
            }
          },
          'image/png',
          1,
        );
      };
      img.onerror = () => {
        return 'Invalid image content.';
      };
      //debugger
      img.src = e?.target?.result as string;
    };
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
              imageCompressorHandler={imageCompressorHandler}
              required={true}
            />
            <FormField
              control={form.control}
              name="name"
              render={({ field }) => (
                <FormItem>
                  <FormLabel>Name</FormLabel>
                  <FormControl>
                    <Input type="text" placeholder="Enter Name" {...field} />
                  </FormControl>
                  <FormMessage />
                </FormItem>
              )}
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
                  <FormMessage />
                </FormItem>
              )}
            />
            <FormField
              control={form.control}
              name="description"
              render={({ field }) => (
                <FormItem>
                  <FormLabel>Enter Description</FormLabel>

                  <FormControl>
                    <Textarea placeholder="Enter description" {...field} />
                  </FormControl>
                  <FormMessage />
                </FormItem>
              )}
            />
          </div>
        </div>
        <div className="flex items-center justify-between">
          <div></div>
          <Button type="submit" variant={'clip'} className="w-1/2">
            {index ? 'Edit Spot Light' : 'Add Spot Light'}
          </Button>
        </div>
      </form>
    </Form>
  );
}
