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
import { NewFileInput } from '../file_input';
import { useState } from 'react';
import { getS3ImageUrl } from '~/service/api/s3Url.service';
import { isValidImageType } from '~/utils/helper';
import { trpc } from '~/utils/trpc';

export default function CategoryForm() {
  const [image, setImage] = useState<File>();

  const addCategory = trpc.category.create.useMutation();

  // 1. Define your form.
  const form = useForm<CreateCategorySchema>({
    resolver: zodResolver(createCategorySchema),
    defaultValues: {
      creator_id: 1,
      thumb: '',
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
    // Do something with the form values.
    // ✅ This will be type-safe and validated.
    console.log(values);

    try {
      if (typeof image === 'undefined') return alert('Please select an image');

      const thumb = await uploadOnS3Handler();
      const payload = { ...values, thumb };

      const response = await addCategory.mutateAsync(payload);
      console.log({ response });
    } catch (error) {
      console.log(error);
    }
  }

  async function imageHandler(originalFile: File) {
    const optimizedFile = await compressImage(originalFile);
    setImage(optimizedFile);
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

  return (
    <Form {...form}>
      <form onSubmit={form.handleSubmit(onSubmit)} className="space-y-4">
        <NewFileInput
          register={form.register('thumb')}
          reset={form.reset}
          getValues={form.getValues}
          setValue={form.setValue}
          onChange={imageHandler}
          onRemove={setImage}
          required={true}
        />

        <div className="grid grid-cols-2 gap-2 w-full">
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
                <FormMessage />
              </FormItem>
            )}
          />
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
                <FormMessage />
              </FormItem>
            )}
          />
        </div>
        <div className="grid grid-cols-2 gap-2 w-full">
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
                <FormMessage />
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
                <FormMessage />
              </FormItem>
            )}
          />
        </div>
        <div className="flex justify-between">
          <div></div>
          <Button type="submit" variant={'clip'}>
            Submit
          </Button>
        </div>
      </form>
    </Form>
  );
}

async function compressImage(fileImage: File) {
  const bitmap = await createImageBitmap(fileImage);
  const canvas = document.createElement('canvas');
  const ctx = canvas.getContext('2d');
  canvas.width = bitmap.width;
  canvas.height = bitmap.height;
  ctx?.drawImage(bitmap, 0, 0);
  // Convert canvas content to a new Blob with reduced quality
  const reducedBlob: Blob = await new Promise((resolve) => {
    canvas.toBlob((blob) => resolve(blob as Blob), 'image/webp', 0.01);
  });

  // Create a new File object from the reduced Blob
  const reducedFile = new File([reducedBlob], fileImage.name, {
    type: 'image/webp', // Adjust the type if needed
    lastModified: fileImage.lastModified,
  });

  return reducedFile;
}
