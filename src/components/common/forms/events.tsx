import { useState } from 'react';
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
import { FileInput, MultiFileInput } from '~/components/common/file_input';
import { trpc } from '~/utils/trpc';
import { getS3ImageUrl } from '~/service/api/s3Url.service';
import { compressImage, isValidImageType } from '~/utils/helper';
import { useToast } from '~/components/ui/use-toast';
import { Textarea } from '~/components/ui/textarea';
import { LoadingDialog } from '../modal/loadingModal';

import { Switch } from '~/components/ui/switch';
import {
  Select,
  SelectContent,
  SelectGroup,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from '~/components/ui/select';
import { Editor } from 'primereact/editor';
import { EventFormSchema } from '~/schema/event';
//theme
import 'primereact/resources/themes/lara-light-indigo/theme.css';
//core
import 'primereact/resources/primereact.min.css';

const formSchema = [
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
    label: 'Category',

    placeholder: 'Please Select Category',
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
    name: 'is_cash_alt',
    label: 'Alternative Selling Option',

    placeholder: 'Please Enter Price',
  },
  {
    type: 'switch_text',
    name: 'cash_alt',
    label: 'Alternative Selling Option',

    placeholder: 'Please Enter Price',
  },
];

type EventImageType = {
  id: number;
  thumb: string;
  event_id: number;
};

export default function EventForm() {
  const { toast } = useToast();
  const { data: categoryData } = trpc.category.getCategory.useQuery({
    lang_id: 1,
  });

  const router = useRouter();
  const [optimizeFile, setOptimizeFile] = useState<any>(null);
  const [optimizeMultiFile, setOptimizeMultiFile] = useState<File[]>([]);
  const [removedImages, setRemovedImages] = useState<EventImageType[]>([]);

  const [isSubmitting, setIsSubmitting] = useState(false);

  const eventId = router.query.index ? +router.query.index : 0;

  const form = useForm<z.infer<typeof EventFormSchema>>({
    resolver: zodResolver(EventFormSchema),
  });

  const { data: eventData } = trpc.event.getEventsById.useQuery(
    { id: eventId },
    {
      enabled: eventId > 0 ? true : false,
      onSuccess: (payload) => {
        console.log('get successfully');
        setFormData(payload);

        // router.push('/store/wallet-connect');
      },
    },
  );

  const eventUpload = trpc.event.create.useMutation({
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
  async function onSubmit(values: z.infer<typeof EventFormSchema>) {
    console.log({ values });
    try {
      setIsSubmitting(true);
      const nftSource =
        typeof form.getValues('thumb') !== 'object'
          ? { thumb: values?.thumb }
          : await uploadOnS3Handler(optimizeFile);
      const multiImage: string[] = optimizeMultiFile.length
        ? await uploadMultiImage()
        : [];
      const payload = { ...values, multi_image: multiImage, ...nftSource };
      const data = await eventUpload.mutateAsync(payload);
      if (data) {
        toast({
          variant: 'success',
          title: 'Event Uploaded Successfully',
        });
        setIsSubmitting(false);
        router.back();
      } else {
        throw new Error('Data Create Error');
      }
    } catch (e: any) {
      setIsSubmitting(false);

      toast({
        variant: 'destructive',
        title: e.message,
      });
    }
  }
  async function uploadMultiImage() {
    const multiImagePromise: Promise<void | {
      thumb: string;
    }>[] = optimizeMultiFile.map((image: File) => uploadOnS3Handler(image));

    const multiImage = await Promise.all(multiImagePromise);
    const data = multiImage.map((image) => image?.thumb ?? '');

    return data;
  }
  async function imageHandler(originalFiles: File[]) {
    const multiImagePromise: Promise<File>[] = originalFiles.map((image) =>
      compressImage(image),
    );
    const multiImage = await Promise.all(multiImagePromise);
    setOptimizeMultiFile((prevImages: File[]) => [
      ...prevImages,
      ...multiImage,
    ]);
  }
  async function singleImageHandler(originalFile: File) {
    console.log(originalFile, 'originalFile');
    const optimizedFile = await compressImage(originalFile);
    // console.log(type, 'typeof originalFile');
    setOptimizeFile(optimizedFile);
  }
  async function uploadOnS3Handler(originalFile: any) {
    if (originalFile?.name) {
      const response = await getS3ImageUrl(originalFile);
      if (!response.success)
        return console.log('response.message', response.message);

      const isImage = isValidImageType(originalFile?.type);

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

  type EventDataType = typeof eventData;

  function setFormData(payload: EventDataType) {
    if (payload?.data) {
      form.setValue('cash_alt', payload.data?.cash_alt);
      form.setValue('category_id', payload.data?.category_id);
      form.setValue('is_cash_alt', payload.data?.is_cash_alt);
      form.setValue('launch_date', payload.data?.launch_date);
      form.setValue('price', payload.data?.price);
      form.setValue('thumb', payload.data?.thumb);
      form.setValue('total_tickets', payload.data?.total_tickets);
      form.setValue('user_ticket_limit', payload.data?.user_ticket_limit);
      form.setValue('video_src', payload.data?.video_src);
    }
  }
  console.log(form.formState.errors, 'form.error');
  const renderHeader = () => {
    return (
      <>
        <span className="ql-formats">
          <select className="ql-header">
            <option selected></option>
            <option value="1"></option>
            <option value="2"></option>
            <option value="3"></option>
          </select>
        </span>
        <span className="ql-formats">
          <button className="ql-bold" aria-label="Bold"></button>
          <button className="ql-italic" aria-label="Italic"></button>
          <button className="ql-underline" aria-label="Underline"></button>
        </span>

        <span className="ql-formats">
          <button className="ql-strike" aria-label="Font"></button>
          <button className="ql-list" value="ordered"></button>
          <button className="ql-list" value="bullet"></button>
          <button aria-label="Link" className="ql-link"></button>
          {/* <button aria-label="Image" className="ql-image"></button> */}
        </span>
        <span className="ql-formats">
          <select className="ql-align">
            <option selected></option>
            <option className="ql-center" value="center"></option>
            <option value="right">right</option>
            <option value="justify"> justify</option>
          </select>
        </span>
      </>
    );
  };

  const header = renderHeader();
  return (
    <>
      <Form {...form}>
        <form
          onSubmit={form.handleSubmit(onSubmit)}
          className="relative justify-center items-center px-8 py-4 space-y-4  overflow-hidden "
        >
          <div className="space-y-4">
            <div className="w-full">
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
                files={optimizeMultiFile}
                setFiles={setOptimizeMultiFile}
                setRemovedImages={setRemovedImages}
                eventImages={eventData?.data?.EventImages}
                required={true}
                placeholder={'Upload Multiple file'}
              />
            </div>
            <div>
              {form.formState.errors?.ar && form.formState.errors?.en && (
                <div className="flex gap-2 items-center p-2  text-destructive bg-white bg-opacity-60 rounded-md">
                  <i className="fa-solid fa-circle-info"></i>Please Fill English
                  & Arabic form
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
              <TabsList>
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
                        <Input
                          type="text"
                          placeholder="Enter Name"
                          {...field}
                        />
                      </FormControl>
                      <FormMessage />
                    </FormItem>
                  )}
                />
                <FormField
                  control={form.control}
                  name="en.desc"
                  render={({ field }) => (
                    <FormItem>
                      <FormLabel>Description</FormLabel>
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
                <FormField
                  control={form.control}
                  name="en.comp_details"
                  render={({ field }) => (
                    <FormItem className=" bg-black">
                      <FormLabel>Competiton Details</FormLabel>
                      <FormControl>
                        <Editor
                          id={field.name}
                          name="blog"
                          value={field.value}
                          className=" bg-black"
                          headerTemplate={header}
                          onTextChange={(e) => field.onChange(e.textValue)}
                          // p-editor-container="bg-black"
                          style={{ height: '320px', backgroundColor: 'black' }}
                        />
                        {/* <Textarea placeholder="Enter Description..." {...field} /> */}
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
                    name="ar.desc"
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
                  <FormField
                    control={form.control}
                    name="ar.comp_details"
                    render={({ field }) => (
                      <FormItem className=" bg-black">
                        <FormLabel>Competiton Details</FormLabel>
                        <FormControl>
                          <div dir="rtl">
                            <Editor
                              id={field.name}
                              name="blog"
                              value={field.value}
                              className=" bg-black"
                              headerTemplate={header}
                              onTextChange={(e) => field.onChange(e.textValue)}
                              // p-editor-container="bg-black"
                              style={{
                                height: '320px',
                                backgroundColor: 'black',
                              }}
                            />
                          </div>
                          {/* <Textarea placeholder="Enter Description..." {...field} /> */}
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
                {formSchema.map((item: any, i: number) => {
                  if (item?.type == 'text' || item?.type == 'number') {
                    return (
                      <FormField
                        key={i}
                        control={form.control}
                        name={item?.name}
                        render={({ field }) => (
                          <FormItem>
                            <FormLabel>{item?.label}</FormLabel>
                            <FormControl>
                              <Input
                                type={item?.type}
                                placeholder={item?.placeholder}
                                {...form.register(item?.name, {
                                  valueAsNumber:
                                    item?.type == 'number' ? true : false,
                                })}
                              />
                            </FormControl>
                            <FormMessage />
                          </FormItem>
                        )}
                      />
                    );
                  }
                  if (item?.type == 'date') {
                    return (
                      <div key={i}>
                        <FormItem className=" flex flex-col gap-2 mt-2 w-full">
                          <FormLabel>{item?.label}</FormLabel>
                          <FormControl>
                            <Input
                              type={item?.type}
                              placeholder={item?.placeholder}
                              {...form.register(item?.name, {
                                valueAsDate: true,
                              })}
                            />
                          </FormControl>
                          <FormMessage />
                        </FormItem>
                      </div>
                    );
                  }
                  if (item?.type == 'switch') {
                    return (
                      <div key={i} className=" h-full">
                        <FormField
                          control={form.control}
                          name={item?.name}
                          render={({ field }) => (
                            <FormItem className="flex items-center justify-between h-full  gap-2">
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
                      <div key={i}>
                        <FormField
                          control={form.control}
                          name={item?.name}
                          render={({ field }) => (
                            <FormItem>
                              <FormLabel>{item?.label}</FormLabel>
                              <Select
                                onValueChange={field.onChange}
                                defaultValue={field.value}
                                value={field.value}
                              >
                                <FormControl>
                                  <SelectTrigger className=" rounded-none  ">
                                    <SelectValue
                                      className="!bg-black"
                                      placeholder={item?.placeholder}
                                    />
                                  </SelectTrigger>
                                </FormControl>
                                <SelectContent className="">
                                  <SelectGroup>
                                    {categoryData &&
                                      categoryData.map(
                                        (item: any, index: number) => {
                                          return (
                                            <div key={index}>
                                              <SelectItem
                                                className="bg-slate-950"
                                                value={(item?.id).toString()}
                                              >
                                                {item?.name}
                                              </SelectItem>
                                            </div>
                                          );
                                        },
                                      )}
                                  </SelectGroup>
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
                      <div key={i}>
                        {form.watch('is_cash_alt') && (
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
                        )}
                      </div>
                    );
                  } else {
                    return <div key={i}></div>;
                  }
                })}
              </div>
            </div>
          </div>
          <div className="flex items-center justify-between">
            <div></div>
            <Button type="submit" variant={'clip'} className="w-1/2">
              {eventId > 0 ? 'Edit Event' : 'Add Event'}
            </Button>
          </div>
        </form>
      </Form>
      <LoadingDialog open={isSubmitting} text={'Saving data...'} />
    </>
  );
}
