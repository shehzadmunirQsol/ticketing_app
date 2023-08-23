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
import { FileInput, MultiFileInput } from '~/components/common/file_input';
import { useEffect, useState } from 'react';
import { trpc } from '~/utils/trpc';
import { getS3ImageUrl } from '~/service/api/s3Url.service';
import { compressImage, isValidImageType } from '~/utils/helper';
import { useToast } from '~/components/ui/use-toast';
import { Textarea } from '~/components/ui/textarea';
import { LoadingDialog } from '../modal/loadingModal';
import { Calendar } from '~/components/ui/calender';
import {
  Popover,
  PopoverContent,
  PopoverTrigger,
} from '~/components/ui/popover';
import { CalendarIcon } from 'lucide-react';
import { cn } from '~/utils/cn';
import { format } from 'date-fns';
import { Switch } from '~/components/ui/switch';
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from '~/components/ui/select';
const formSchema: any = [
  {
    type: 'image',
    name: 'thumb',
    label: 'Image',

    placeholder: 'Please Select Image',
  },
  {
    type: 'image',
    name: 'multi_image',
    label: 'Multi Image',

    placeholder: 'Please Select Image',
  },
  {
    type: 'number',
    name: 'price',
    label: 'Token Price',

    placeholder: 'Please Enter Token Price',
  },
  {
    type: 'select',
    name: 'category_id',
    label: 'Category ID',

    placeholder: 'Please Enter Token Price',
  },
  {
    type: 'text',
    name: 'video_src',
    label: 'Video Source',

    placeholder: 'Please Enter Video Source',
  },
  {
    type: 'number',
    name: 'total_tickets',
    label: 'Total Cap',
    placeholder: 'Please Enter Total Cap',
  },
  {
    type: 'number',
    name: 'user_ticket_limit',
    label: 'Per User Cap',
    placeholder: 'Please Enter Per User Cap',
  },

  {
    type: 'date',
    name: 'launch_date',
    label: 'Launch Date',

    placeholder: 'Please Enter Price',
  },
  {
    type: 'date',
    name: 'end_date',
    label: 'End Date',

    placeholder: 'Please Enter Price',
  },
  {
    type: 'switch',
    name: 'is_alt',
    label: 'Alternative Selling Option',

    placeholder: 'Please Enter Price',
  },
  {
    type: 'switch_text',
    name: 'is_alternative_option',
    label: 'Alternative Selling Option',

    placeholder: 'Please Enter Price',
  },
];
const SpotLightFormSchema = z.object({
  thumb: z.any(),
  multi_image: z.any(),
  price: z.any(),
  video_src: z.string(),
  link: z.string(),
  total_tickets: z.string(),
  user_ticket_limit: z.string(),
  is_alt: z.boolean(),
  launch_date: z.date(),
  end_date: z.date(),

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
    name: z.string(),
    description: z.string().optional(),
  }),
  ar: z
    .object({
      name: z.string(),
      description: z.string().optional(),
    })
    .optional(),
});
const arFormSchema = z.object({
  thumb: z.any(),
  link: z.string(),
  en: z
    .object({
      name: z.string(),
      description: z.string().optional(),
    })
    .optional(),
  ar: z.object({
    name: z.string(),
    description: z.string().optional(),
  }),
});
export function EventForm() {
  const { toast } = useToast();

  const router = useRouter();
  const [optimizeFile, setOptimizeFile] = useState<any>(null);
  const [optimizeMultiFile, setOptimizeMultiFile] = useState<any>(null);
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
        form.setValue('en.name', json_data?.name);
        form.setValue('en.description', json_data?.description);
      } else {
        form.setValue('ar.name', json_data?.name);
        form.setValue('ar.description', json_data?.description);
      }
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
    console.log({ values });
    try {
      // setIsSubmitting(true);

      const nftSource =
        typeof form.getValues('thumb') !== 'object'
          ? { thumb: values?.thumb }
          : await uploadOnS3Handler();
      const multiImage =
        typeof form.getValues('multi_image') !== 'object'
          ? { multi_image: values?.multi_image }
          : await uploadMultiImage();
      const payload: any = { ...values };
      // if (index) {
      //   let dataPayload: any;
      //   if (editData?.lang_id == 1) {
      //     dataPayload = {
      //       id: +index,

      //       lang_id: 1,

      //       group: 'WONDER',
      //       key: 'spotlight',
      //       value: JSON.stringify({
      //         ...nftSource,
      //         link: values?.link,
      //         ...values?.en,
      //       }),
      //     };
      //   } else {
      //     dataPayload = {
      //       id: +index,

      //       lang_id: 2,

      //       group: 'WONDER',
      //       key: 'spotlight',
      //       value: JSON.stringify({
      //         ...nftSource,
      //         link: values?.link,
      //         ...values?.ar,
      //       }),
      //     };
      //   }

      //   const data = await bannerUpdate.mutateAsync({ ...dataPayload });
      //   if (data) {
      //     setIsSubmitting(false);

      //     toast({
      //       variant: 'success',
      //       title: 'Banner Updated Successfully',
      //     });
      //     router.back();
      //   } else {
      //     throw new Error('Data update Error');
      //   }
      // } else {
      //   const dataPayload = [
      //     {
      //       lang_id: 1,
      //       group: 'WONDER',
      //       key: 'spotlight',
      //       value: JSON.stringify({
      //         ...nftSource,
      //         link: values?.link,
      //         ...values?.en,
      //       }),
      //     },
      //     {
      //       lang_id: 2,
      //       group: 'WONDER',
      //       key: 'spotlight',
      //       value: JSON.stringify({
      //         ...nftSource,
      //         link: values?.link,
      //         ...values?.ar,
      //       }),
      //     },
      //   ];
      //   const data = await bannerUpload.mutateAsync(dataPayload);
      //   if (data) {
      //     setIsSubmitting(false);

      //     toast({
      //       variant: 'success',
      //       title: 'Banner Uploaded Successfully',
      //     });
      //     router.back();
      //   } else {
      //     throw new Error('Data Create Error');
      //   }
      // }
    } catch (e: any) {
      setIsSubmitting(false);

      toast({
        variant: 'destructive',
        title: e.message,
      });
    }
  }
  async function uploadMultiImage() {
    if (optimizeMultiFile) {
    }
  }
  async function imageHandler(originalFile: any[], type: any) {
    const multi_Image: any[] = [];
    // const optimizedFile = await compressImage(originalFile);
    for (let i = 0; i < originalFile.length; i++) {
      const single_image = await compressImage(originalFile[i]);
      console.log({ single_image });
      multi_Image.push(single_image);
    }
    setOptimizeMultiFile(multi_Image);

    console.log(multi_Image, 'typeof originalFile');
    // setOptimizeFile(optimizedFile);
  }
  async function singleImageHandler(originalFile: File) {
    console.log(originalFile, 'originalFile');
    const optimizedFile = await compressImage(originalFile);
    // console.log(type, 'typeof originalFile');
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
        className="relative justify-center items-center px-8 py-4 space-y-4  overflow-hidden "
      >
        <div className="space-y-4">
          <div>
            <FileInput
              register={form.register('thumb')}
              reset={form.reset}
              getValues={form.getValues}
              setValue={form.setValue}
              imageCompressorHandler={singleImageHandler}
              placeholder={'Upload Single file'}
              required={true}
            />
            <MultiFileInput
              register={form.register('multi_image')}
              reset={form.reset}
              getValues={form.getValues}
              setValue={form.setValue}
              imageCompressorHandler={imageHandler}
              required={true}
              placeholder={'Upload Multiple file'}
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
                name="en.name"
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
                name="en.description"
                render={({ field }) => (
                  <FormItem>
                    <FormLabel>Description</FormLabel>
                    <FormControl>
                      <Textarea placeholder="Enter Description..." {...field} />
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
                      <FormMessage />
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
                        <Textarea
                          placeholder="Enter Description..."
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
          <div>
            <div className=" grid grid-cols-1 lg:grid-cols-2 gap-2  items-center">
              {formSchema &&
                formSchema.map((item: any, index: number) => {
                  if (item?.type == 'text' || item?.type == 'number') {
                    return (
                      <div key={index}>
                        <FormField
                          control={form.control}
                          name={item?.name}
                          render={({ field }) => (
                            <FormItem>
                              <FormLabel>{item?.label}</FormLabel>
                              <FormControl>
                                <Input
                                  type={item?.type}
                                  placeholder={item?.placeholder}
                                  {...field}
                                />
                              </FormControl>
                              <FormMessage />
                            </FormItem>
                          )}
                        />
                      </div>
                    );
                  }
                  if (item?.type == 'date') {
                    return (
                      <div key={index}>
                        <FormField
                          control={form.control}
                          name={item?.name}
                          render={({ field }) => (
                            <FormItem className=" flex flex-col gap-2 mt-2 w-full">
                              <FormLabel>{item?.label}</FormLabel>
                              <FormControl>
                                <Popover>
                                  <PopoverTrigger asChild>
                                    <Button
                                      variant={'outline'}
                                      className={`w-full   justify-start text-left font-normal 
                              ${
                                !field.value && 'text-muted-foreground w-full'
                              }`}
                                    >
                                      <CalendarIcon className="mr-2 h-4 w-4" />
                                      {field.value ? (
                                        format(field.value, 'PPP')
                                      ) : (
                                        <span>Pick a date</span>
                                      )}
                                    </Button>
                                  </PopoverTrigger>
                                  <PopoverContent className="w-auto p-0">
                                    <Calendar
                                      mode="single"
                                      selected={field.value}
                                      onSelect={field.onChange}
                                      disabled={(date) =>
                                        date < new Date() ||
                                        date < new Date('1900-01-01')
                                      }
                                      initialFocus
                                    />
                                  </PopoverContent>
                                </Popover>
                              </FormControl>
                              <FormMessage />
                            </FormItem>
                          )}
                        />
                      </div>
                    );
                  }
                  if (item?.type == 'switch') {
                    return (
                      <div key={index}>
                        <FormField
                          control={form.control}
                          name={item?.name}
                          render={({ field }) => (
                            <FormItem className="flex items-center gap-2">
                              <FormLabel>Alternative Selling Option</FormLabel>
                              <FormControl>
                                <Switch
                                  checked={field.value}
                                  onCheckedChange={field.onChange}
                                />
                              </FormControl>
                              <FormMessage />
                            </FormItem>
                          )}
                        />
                      </div>
                    );
                  }
                  if (item?.type == 'select') {
                    return (
                      <div key={index}>
                        <FormField
                          control={form.control}
                          name={item?.name}
                          render={({ field }) => (
                            <FormItem>
                              <FormLabel>{item?.label}</FormLabel>
                              <Select
                                onValueChange={field.onChange}
                                defaultValue={field.value}
                              >
                                <FormControl>
                                  <SelectTrigger className=" rounded-none  ">
                                    <SelectValue
                                      placeholder={item?.placeholder}
                                    />
                                  </SelectTrigger>
                                </FormControl>
                                <SelectContent>
                                  <SelectItem value="m@example.com">
                                    m@example.com
                                  </SelectItem>
                                  <SelectItem value="m@google.com">
                                    m@google.com
                                  </SelectItem>
                                  <SelectItem value="m@support.com">
                                    m@support.com
                                  </SelectItem>
                                </SelectContent>
                              </Select>

                              <FormMessage />
                            </FormItem>
                          )}
                        />
                      </div>
                    );
                  }
                  if (item?.type == 'switch_text') {
                    return (
                      <div key={index}>
                        {form.watch('is_alt') && (
                          <FormField
                            control={form.control}
                            name="link"
                            render={({ field }) => (
                              <FormItem>
                                <FormLabel>{item?.label}</FormLabel>
                                <FormControl>
                                  <Input
                                    type={item?.type}
                                    placeholder={item?.placeholder}
                                    {...field}
                                  />
                                </FormControl>
                                <FormMessage />
                              </FormItem>
                            )}
                          />
                        )}
                      </div>
                    );
                  }
                })}
            </div>
          </div>
        </div>
        <div className="flex items-center justify-between">
          <div></div>
          <Button type="submit" variant={'clip'} className="w-1/2">
            {index ? 'Edit Event' : 'Add Event'}
          </Button>
        </div>
      </form>
      <LoadingDialog open={isSubmitting} text={'Saving data...'} />
    </Form>
  );
}
