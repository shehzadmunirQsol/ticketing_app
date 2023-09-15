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
import { cmsSchemaInput, cmsSchema } from '~/schema/cms';
import { ImageInput } from '../file_input';
import { useEffect, useState } from 'react';
import { getS3ImageUrl } from '~/service/api/s3Url.service';
import { trpc } from '~/utils/trpc';
import { useRouter } from 'next/router';
import { compressImage, createSlug } from '~/utils/helper';
import { LoadingDialog } from '../modal/loadingModal';
import { LanguageInterface } from '../language_select';
// import { Editor } from 'primereact/editor';
import CKEditor from 'react-ckeditor-component';

//theme
import 'primereact/resources/themes/lara-light-indigo/theme.css';
//core
import 'primereact/resources/primereact.min.css';
import { useToast } from '~/components/ui/use-toast';
import Link from 'next/link';

interface CategoryFormInterface {
  language: LanguageInterface;
}

export default function CmsForm(props: CategoryFormInterface) {
  const router = useRouter();
  const { toast } = useToast();
  const { id = 0 } = router.query;
  const form = useForm<any>();
  // const form = useForm<cmsSchemaInput>({
  // resolver: zodResolver(cmsSchema),
  // });

  const [image, setImage] = useState<File>();
  const [loading, setLoading] = useState<boolean>(false);
  const [content, setContent] = useState<any>('');

  // Getting Data
  const { data } = trpc.cms.getById.useQuery(
    { id: +id },
    {
      refetchOnWindowFocus: false,
      enabled: id ? true : false,
      onSuccess(res) {
        console.log(res, 'res data');
        console.log(data?.data?.CMSDescription[0]?.title, '');
      },
    },
  );

  useEffect(() => {
    if (data) {
      form.setValue('en.title', data?.data?.CMSDescription[0]?.title);
      form.setValue('en.slug', data?.data?.slug as string);
      form.setValue('en.desc', data?.data?.CMSDescription[0]?.desc as string);
      form.setValue(
        'en.metadesc',
        data?.data?.CMSDescription[0]?.meta_keywords as string,
      );
      form.setValue(
        'en.metatitle',
        data?.data?.CMSDescription[0]?.meta_keywords as string,
      );
      setContent(data?.data?.CMSDescription[0]?.content);
    }
  }, [data]);

  // Handle the change event when the CKEditor content changes
  const handleChange = (evt: any) => {
    const newContent: any = evt.editor.getData();

    setContent(newContent);
  };
  // updating data
  const updateCmsId = trpc.cms.updateById.useMutation({
    onSuccess: (res) => {
      console.log(res);
      toast({
        variant: 'success',
        title: 'cms Updated Successfully',
      });
      // setEdit(false);
    },
    onError(error) {
      console.log(error);
    },
  });

  // cms customer
  const AddCmsContent = trpc.cms.addCmsContent.useMutation({
    onSuccess: (res: any) => {
      console.log(res, 'REs');
    },
    onError: (err) => {
      console.log(err.message, 'err');
    },
  });

  // 2. Define a submit handler.
  async function onSubmit(values: any) {
    console.log('add HSJSJSJSHJ');
    console.log(values, 'values');
    try {
      const slug = createSlug(values?.en?.slug);
      console.log(slug);
      const payload: any = {
        content: content,
        slug: slug,
        ...values,
      };
      console.log(payload, 'payload');
      const result = await AddCmsContent.mutateAsync(payload);
      console.log(result, 'cmsAboutUs');
      toast({
        variant: 'success',
        title: 'Cms Content Add Successfully',
      });
      localStorage.removeItem('cmscontent');
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
  }

  // update data andler
  const onSubmitUpdate = async (values: any) => {
    try {
      const slug = createSlug(values?.en?.slug);
      const payload = {
        id: +id,
        content: content,
        slug: slug,
        ...values,
      };
      console.log(payload, 'payload HSJSJSJSHJ');
      const result = await updateCmsId.mutateAsync(payload);
      console.log(result, 'cmsAboutUs HSJSJSJSHJ');
      toast({
        variant: 'success',
        title: 'Cms Content Update Successfully',
      });
      localStorage.removeItem('cmscontent');
      form.setValue('title', '');
      form.setValue('slug', '');
      form.setValue('metatitle', '');
      form.setValue('metadesc', '');
      form.setValue('desc', '');
      form.setValue('content', '');
      router.push('/admin/cms');
    } catch (error: any) {
      toast({
        variant: 'destructive',
        title: error.message,
      });
    }
  };

  // language handle
  const langError =
    form.formState.errors?.en && form.formState.errors?.ar
      ? 'Kindly provide information in both English and Arabic fields.'
      : form.formState.errors?.en && !form.formState.errors?.ar
        ? 'Kindly provide information in English fields.'
        : !form.formState.errors?.en && form.formState.errors?.ar
          ? 'Kindly provide information in Arabic fields.'
          : '';

  // Handle CMS Preview
  const handlePreview = () => {
    window.localStorage.setItem('cmscontent', content);
    window.open('/admin/cms/preview', '_blank');
  };

  // Editor Config
  const editorConfig = {
    stylesSet: 'default',
    allowedContent: true,
    autoParagraph: false,
    extraAllowedContent: 'b i div class style',
  };

  return (
    <Form {...form}>
      <form
        onSubmit={form.handleSubmit(id ? onSubmitUpdate : onSubmit)}
        className="space-y-4"
      >
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
              ? 'block lg:space-y-4 md:space-y-4  space-y-8'
              : 'hidden lg:space-y-4 md:space-y-4 space-y-8'
          }
        >
          <div className="flex flex-col lg:flex-row md:flex-row justify-between  lg:space-x-8 md:space-x-8  s">
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

                  <div className='relative pb-2'>
                    <FormMessage />
                  </div>
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

                  <div className='relative pb-2'>
                    <FormMessage />
                  </div>
                </FormItem>
              )}
            />
          </div>

          <div className="flex flex-col lg:flex-row md:flex-row justify-between lg:space-x-8 md:space-x-8 ">
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

                  <div className='relative pb-2'>
                    <FormMessage />
                  </div>
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

                  <div className='relative pb-2'>
                    <FormMessage />
                  </div>
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

                <div className='relative pb-2'>
                  <FormMessage />
                </div>
              </FormItem>
            )}
          />

          <FormField
            control={form.control}
            name="en.content"
            render={({ field }) => (
              <FormItem className=" text-black">
                <FormLabel>
                  Content <sup className="text-md text-red-500">*</sup>
                </FormLabel>
                <FormControl>
                  <CKEditor
                    activeClass="p10"
                    content={content} // Set the initial content
                    events={{
                      change: handleChange,
                    }}
                    config={editorConfig}
                  />
                </FormControl>

                <div className='relative pb-2'>
                  <FormMessage />
                </div>
              </FormItem>
            )}
          />
        </div>
        <div className="flex justify-end gap-4">
          <div></div>

          <Button type="button" variant={'clip'} onClick={handlePreview}>
            Preview
          </Button>

          <Button type="submit" variant={'clip'}>
            {id ? 'Update' : 'Submit'}
          </Button>
        </div>
        <LoadingDialog
          open={AddCmsContent.isLoading || loading}
          text={`${id ? 'Updating' : 'Adding'} CMS...`}
        />
        <LoadingDialog
          open={updateCmsId.isLoading || loading}
          text={`${id ? 'Updating' : 'Adding'} CMS...`}
        />
      </form>
    </Form>
  );
}
