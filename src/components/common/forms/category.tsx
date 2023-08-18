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
import { Textarea } from '@/ui/textarea';
import { useForm } from 'react-hook-form';
import { createCategorySchema } from '~/schema/category';

export default function CategoryForm() {
  // 1. Define your form.
  const form = useForm<z.infer<typeof createCategorySchema>>({
    resolver: zodResolver(createCategorySchema),
    defaultValues: {
      creator_id: 1,
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
  function onSubmit(values: z.infer<typeof createCategorySchema>) {
    // Do something with the form values.
    // ✅ This will be type-safe and validated.
    console.log(values);
  }

  return (
    <Form {...form}>
      <form onSubmit={form.handleSubmit(onSubmit)} className="space-y-4">
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

async function compressImage(event: React.ChangeEvent<HTMLInputElement>) {
  const blobImg = event?.target?.files && event?.target?.files[0];
  const bitmap = await createImageBitmap(blobImg as File);
  const canvas = document.createElement('canvas');
  const ctx = canvas.getContext('2d');
  canvas.width = bitmap.width;
  canvas.height = bitmap.height;
  ctx?.drawImage(bitmap, 0, 0);
  const dataUrl = canvas.toDataURL('image/png', 100 / 100);
  return dataUrl;
}
