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
import { LanguageInterface } from '../language_select';
import { Editor } from 'primereact/editor';
//theme
import 'primereact/resources/themes/lara-light-indigo/theme.css';
//core
import 'primereact/resources/primereact.min.css';
import { useToast } from '~/components/ui/use-toast';
import Link from 'next/link';

interface CategoryFormInterface {
  language: LanguageInterface;
}

export default function cmsForm(props: CategoryFormInterface) {
  const [image, setImage] = useState<File>();
  const [loading, setLoading] = useState<boolean>(false);

  const router = useRouter();
  const { toast } = useToast();

  const { id = 0 } = router.query;

  const { data } = trpc.cms.getById.useQuery(
    { id: +id },
    {
      refetchOnWindowFocus: false,
      enabled: id ? true : false,
      onSuccess(res) {
       console.log(res,"res data")

        form.setValue('en.title', data?.data?.CMSDescription[0]?.title);
        form.setValue('en.slug', data?.data?.slug as string);
        form.setValue('en.desc', data?.data?.CMSDescription[0]?.desc as string);
        form.setValue('en.metadesc', data?.data?.CMSDescription[0]?.meta_keywords as string);
        form.setValue('en.metatitle', data?.data?.CMSDescription[0]?.meta_keywords as string);
        form.setValue('en.content', data?.data?.CMSDescription[0]?.content as string);
      },
    },
  );
  console.log(data,"data data ")


  // 1. Define your form.
  const form = useForm<any>();

  // cms customer
  const cmsAboutUs = trpc.cms.addCmsContent.useMutation({
    onSuccess: (res: any) => {
      console.log(res, 'REs');
    },
    onError: (err) => {
      console.log(err.message, 'err');
    },
  });

  // 2. Define a submit handler.
  async function onSubmit(values: any) {
    console.log(values, 'values');
    try {
      const result = await cmsAboutUs.mutateAsync(values);
      console.log(result, 'cmsAboutUs');
      toast({
        variant: 'success',
        title: 'Cms Content Add Successfully',
      });
      form.setValue('title', '');
      form.setValue('slug', '');
      form.setValue('metatitle', '');
      form.setValue('metadesc', '');
      form.setValue('desc', '');
      form.setValue('content', '');
      router.back();
    } catch (error: any) {
      toast({
        variant: 'destructive',
        title: error.message,
      });
    }

    // try {
    //   setLoading(true);

    //   if (values.thumb === '') {
    //     if (typeof image === 'undefined')
    //       return alert('Please select an image');
    //     const thumb = await uploadOnS3Handler();
    //     values.thumb = thumb;
    //   }

    //   let response;

    //   if (categoryId) {
    //     response = await updateCategory.mutateAsync({
    //       ...values,
    //       category_id: +categoryId,
    //     });
    //   } else {
    //     response = await addCategory.mutateAsync(values);
    //   }

    //   router.replace('/admin/category');
    //   console.log({ response });
    // } catch (error) {
    //   setLoading(false);

    //   console.log(error);
    // }
  }

  const langError =
    form.formState.errors?.en && form.formState.errors?.ar
      ? 'Kindly provide information in both English and Arabic fields.'
      : form.formState.errors?.en && !form.formState.errors?.ar
      ? 'Kindly provide information in English fields.'
      : !form.formState.errors?.en && form.formState.errors?.ar
      ? 'Kindly provide information in Arabic fields.'
      : '';

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

  const handlePreview = () => {
    const data = form?.watch('en.content') 
    console.log(form?.watch('en.content'),"form.watch")
    router.push({
        pathname: "/admin/cms/preview",
        query: { data: data },
      }, '/admin/cms/preview');
  }
  



  return (
    <Form {...form}>
      <form onSubmit={form.handleSubmit(onSubmit)} className="space-y-4">
        {langError ? (
          <div className="flex gap-2 items-center p-2  text-destructive bg-white bg-opacity-60 rounded-md">
            <i className="fa-solid fa-circle-info"></i>
            {langError}
          </div>
        ) : (
          ''
        )}

        <div
          className={
            props.language.code === 'en'
              ? 'block space-y-4'
              : 'hidden space-y-4'
          }
        >
          <div className="flex flex-col lg:flex-row md:flex-row justify-between space-x-8">
            <FormField
              control={form.control}
              name="en.title"
              render={({ field }) => (
                <FormItem className="w-full">
                  <FormLabel>
                    Title<sup className="text-md text-red-500">*</sup>
                  </FormLabel>
                  <FormControl>
                    <Input placeholder="Enter Page Title" {...field} />
                  </FormControl>
                  <FormMessage />
                </FormItem>
              )}
            />
            <FormField
              control={form.control}
              name="en.slug"
              render={({ field }) => (
                <FormItem className="w-full">
                  <FormLabel>
                    Slug<sup className="text-md text-red-500">*</sup>
                  </FormLabel>
                  <FormControl>
                    <Input placeholder="Enter Page Slug" {...field} />
                  </FormControl>
                  <FormMessage />
                </FormItem>
              )}
            />
          </div>

          <div className="flex flex-col lg:flex-row md:flex-row justify-between space-x-8">
            <FormField
              control={form.control}
              name="en.metatitle"
              render={({ field }) => (
                <FormItem className="w-full">
                  <FormLabel>
                    Meta Title <sup className="text-md text-red-500">*</sup>
                  </FormLabel>
                  <FormControl>
                    <Textarea placeholder="Enter Meta Title..." {...field} />
                  </FormControl>
                  <FormMessage />
                </FormItem>
              )}
            />
            <FormField
              control={form.control}
              name="en.metadesc"
              render={({ field }) => (
                <FormItem className="w-full">
                  <FormLabel>
                    Meta Description{' '}
                    <sup className="text-md text-red-500">*</sup>
                  </FormLabel>
                  <FormControl>
                    <Textarea
                      placeholder="Enter Meta Description..."
                      {...field}
                    />
                  </FormControl>
                  <FormMessage />
                </FormItem>
              )}
            />
          </div>
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
            name="en.content"
            render={({ field }) => (
              <FormItem className=" bg-black">
                <FormLabel>Content*</FormLabel>
                <FormControl>
                  <Editor
                    id={field.name}
                    name="blog"
                    value={field.value}
                    className="bg-black"
                    // headerTemplate={header}
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
        </div>
        {/* <div
          dir="ltr"
          className={props.language.code === 'ar' ? 'block' : 'hidden'}
        >
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
        </div> */}
        <div className="flex justify-end gap-4">
          <div></div>

          <Button type="button" variant={'clip'} onClick={handlePreview}>
            Preview

          </Button>

          <Button type="submit" variant={'clip'}>
            Submit
          </Button>
        </div>
      </form>

      {/* <LoadingDialog
        open={addCategory.isLoading || loading}
        text={`${categoryId ? 'Updating' : 'Adding'} Category...`}
      /> */}
    </Form>
  );
}
