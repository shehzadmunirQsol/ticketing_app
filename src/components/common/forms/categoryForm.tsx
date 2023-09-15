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
import { CreateCategorySchema, createCategorySchema } from '~/schema/category';
import { ImageInput } from '../file_input';
import { useState } from 'react';
import { getS3ImageUrl } from '~/service/api/s3Url.service';
import { trpc } from '~/utils/trpc';
import { useRouter } from 'next/router';
import { compressImage } from '~/utils/helper';
import { LoadingDialog } from '../modal/loadingModal';
import { Tabs, TabsContent, TabsList, TabsTrigger } from '~/components/ui/tabs';

export default function CategoryForm() {
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
        const enData = categoryData?.data.CategoryDescription.find(
          (cat) => cat.lang_id === 1,
        );
        const arData = categoryData?.data.CategoryDescription.find(
          (cat) => cat.lang_id !== 1,
        );

        form.setValue('thumb', categoryData?.data?.thumb);
        form.setValue('en.name', enData?.name as string);
        form.setValue('en.desc', enData?.desc as string);
        form.setValue('ar.name', arData?.name as string);
        form.setValue('ar.desc', arData?.desc as string);
      },
    },
  );

  const addCategory = trpc.category.create.useMutation();
  const updateCategory = trpc.category.update.useMutation();

  // 1. Define your form.
  const form = useForm<CreateCategorySchema>({
    resolver: zodResolver(createCategorySchema),
    defaultValues: {
      user_id: 1,
      thumb: category?.data?.thumb ?? '',
      en: {
        name: '',
        desc: '',
        lang_id: 1,
      },
      ar: {
        name: '',
        desc: '',
        lang_id: 2,
      },
    },
  });

  // 2. Define a submit handler.
  async function onSubmit(values: CreateCategorySchema) {
    try {
      setLoading(true);

      if (values.thumb === '') {
        if (typeof image === 'undefined')
          return alert('Please select an image');
        const thumb = await uploadOnS3Handler();
        values.thumb = thumb;
      }

      let response;

      if (categoryId) {
        response = await updateCategory.mutateAsync({
          ...values,
          category_id: +categoryId,
        });
      } else {
        response = await addCategory.mutateAsync(values);
      }

      router.replace('/admin/category');
      console.log({ response });
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

  const langError =
    form.formState.errors?.en && form.formState.errors?.ar
      ? 'Kindly provide information in both English and Arabic language.'
      : form.formState.errors?.en && !form.formState.errors?.ar
        ? 'Kindly provide information in English language.'
        : !form.formState.errors?.en && form.formState.errors?.ar
          ? 'Kindly provide information in Arabic language.'
          : '';

  return (
    <Form {...form}>
      <form onSubmit={form.handleSubmit(onSubmit)} className="space-y-4">
        <ImageInput
          register={form.register('thumb')}
          reset={form.reset}
          getValues={form.getValues}
          setValue={form.setValue}
          onChange={imageHandler}
          onRemove={setImage}
          required={true}
        />

        {langError ? (
          <div className="flex gap-2 items-center p-2  text-destructive bg-white bg-opacity-60 rounded-md">
            <i className="fa-solid fa-circle-info"></i>
            {langError}
          </div>
        ) : (
          ''
        )}

        <Tabs defaultValue={'en'} className="w-full">
          <TabsList>
            <TabsTrigger value="en">English</TabsTrigger>
            <TabsTrigger value="ar">Arabic</TabsTrigger>
          </TabsList>
          <TabsContent value="en">
            <div>
              <FormField
                control={form.control}
                name="en.name"
                render={({ field }) => (
                  <FormItem>
                    <FormLabel>
                      Name <sup className="text-md text-red-500">*</sup>
                    </FormLabel>
                    <FormControl>
                      <Input placeholder="Enter Category Name" {...field} />
                    </FormControl>

                    <div className='relative pb-2'>
                      <FormMessage />
                    </div>
                  </FormItem>
                )}
              />
              <FormField
                control={form.control}
                name="en.desc"
                render={({ field }) => (
                  <FormItem>
                    <FormLabel>
                      Description <sup className="text-md text-red-500">*</sup>
                    </FormLabel>
                    <FormControl>
                      <Textarea placeholder="Enter Description..." {...field} />
                    </FormControl>

                    <div className='relative pb-2'>
                      <FormMessage />
                    </div>
                  </FormItem>
                )}
              />
            </div>
          </TabsContent>
          <TabsContent value="ar">
            <div dir="ltr">
              <FormField
                control={form.control}
                name="ar.name"
                render={({ field }) => (
                  <FormItem dir="rtl">
                    <FormLabel>
                      اسم <sup className="text-md text-red-500">*</sup>
                    </FormLabel>
                    <FormControl>
                      <Input placeholder="Enter Category Name" {...field} />
                    </FormControl>

                    <div className='relative pb-2'>
                      <FormMessage />
                    </div>
                  </FormItem>
                )}
              />

              <FormField
                control={form.control}
                name="ar.desc"
                render={({ field }) => (
                  <FormItem dir="rtl">
                    <FormLabel>
                      وصف <sup className="text-md text-red-500">*</sup>
                    </FormLabel>
                    <FormControl>
                      <Textarea placeholder="...أدخل الوصف" {...field} />
                    </FormControl>

                    <div className='relative pb-2'>
                      <FormMessage />
                    </div>
                  </FormItem>
                )}
              />
            </div>
          </TabsContent>
        </Tabs>

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
