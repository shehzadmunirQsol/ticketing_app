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
import { useState } from 'react';

const loginFormSchema = z.object({
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

export function EventForm() {
  const router = useRouter();
  const [optimizeFile, setOptimizeFile] = useState<any>(null);

  // 1. Define your form.
  const form = useForm<z.infer<typeof loginFormSchema>>({
    resolver: zodResolver(loginFormSchema),
    defaultValues: {
      thumb: '',
      link: '',

      en: {
        model: '',
        title: '',
        price: '',
        description: '',
        date: '',
      },
      ar: {
        model: '',
        title: '',
        price: '',
        description: '',
        date: '',
      },
    },
  });

  // 2. Define a submit handler.
  function onSubmit(values: z.infer<typeof loginFormSchema>) {
    // Do something with the form values.
    // ✅ This will be type-safe and validated.
    // router.push('/admin/dashboard');

    console.log({ values });
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
                type: 'image/jpeg',
                lastModified: Date.now(),
              });
              // setFile(originalFile);
              setOptimizeFile(optimizeFile);
            }
          },
          'image/jpeg',
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
  return (
    <Form {...form}>
      <form
        onSubmit={form.handleSubmit(onSubmit)}
        className="justify-center items-center px-8 py-4"
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
          <Tabs defaultValue="en" className="w-full">
            <TabsList>
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
                name="ar.title"
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
                name="ar.price"
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
                name="ar.description"
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
                name="ar.date"
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
          </Tabs>
        </div>
        <div className="flex items-center justify-between">
          <div></div>
          <Button type="submit" variant={'clip'} className="w-28">
            Add Event
          </Button>
        </div>
      </form>
    </Form>
  );
}
