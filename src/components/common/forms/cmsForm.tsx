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
import { useForm } from 'react-hook-form';
import { cmsSchemaInput, cmsSchema, cmsSchemaForm } from '~/schema/cms';
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
import {
  Select,
  SelectItem,
  SelectTrigger,
  SelectContent,
  SelectGroup,
  SelectValue,
} from '@/ui/select';

//theme
import 'primereact/resources/themes/lara-light-indigo/theme.css';
//core
import 'primereact/resources/primereact.min.css';
import { useToast } from '~/components/ui/use-toast';
import Link from 'next/link';
import { Tabs, TabsContent, TabsList, TabsTrigger } from '~/components/ui/tabs';
import { Textarea } from '~/components/ui/textarea';

interface CategoryFormInterface {
  language: LanguageInterface;
}

export default function CmsForm(props: CategoryFormInterface) {
  const router = useRouter();
  const { toast } = useToast();
  const { id = 0 } = router.query;

  const form = useForm<cmsSchemaForm>({
    resolver: zodResolver(cmsSchema),
  });

  const [image, setImage] = useState<File>();
  const [loading, setLoading] = useState<boolean>(false);
  const [contentEn, setContentEn] = useState<any>('');
  const [contentAr, setContentAr] = useState<any>('');
  console.log(form.formState.errors, 'form.formState.errors');
  console.log(form.getValues(), 'form.formState.errors');
  // Getting Data
  const { data, isFetching } = trpc.cms.getById.useQuery(
    { id: +id },
    {
      refetchOnWindowFocus: false,
      enabled: id ? true : false,
      onSuccess(res: any) {
        form.setValue('slug', res?.data?.slug as string);
        form.setValue('type', res?.data?.type as any);
        form.setValue(
          'en.title',
          res?.data?.CMSDescription[0]?.title as string,
        );
        form.setValue('en.desc', res?.data?.CMSDescription[0]?.desc as string);
        form.setValue(
          'en.meta_keywords',
          res?.data?.CMSDescription[0]?.meta_keywords as string,
        );
        form.setValue(
          'en.content',
          res?.data?.CMSDescription[0]?.content as string,
        );
        form.setValue('ar.title', res?.data?.CMSDescription[1]?.title);
        form.setValue('ar.desc', res?.data?.CMSDescription[1]?.desc as string);
        form.setValue(
          'ar.meta_keywords',
          res?.data?.CMSDescription[1]?.meta_keywords as string,
        );
        form.setValue(
          'ar.content',
          res?.data?.CMSDescription[1]?.content as string,
        );
        setContentEn(res?.data?.CMSDescription[0]?.content);
        setContentAr(res?.data?.CMSDescription[1]?.content);
      },
    },
  );

  // Handle the change event when the CKEditor content changes
  const handleArChange = (evt: any) => {
    const newContent: any = evt.editor.getData();
    setContentAr(newContent);

    form.setValue('ar.content', newContent);
  };
  const handleEnChange = (evt: any) => {
    const newContent: any = evt.editor.getData();
    setContentEn(newContent);

    form.setValue('en.content', newContent);
  };
  // console.log({ content });
  // updating data
  const updateCmsId = trpc.cms.updateById.useMutation({
    onSuccess: (res: any) => {
      console.log(res);
      toast({
        variant: 'success',
        title: 'cms Updated Successfully',
      });
      // setEdit(false);
    },
    onError(error: any) {
      console.log(error);
    },
  });

  // cms customer
  const AddCmsContent = trpc.cms.addCmsContent.useMutation({
    onSuccess: (res: any) => {
      console.log(res, 'REs');
    },
    onError: (err: any) => {
      console.log(err.message, 'err');
    },
  });

  // 2. Define a submit handler.
  async function onSubmit(values: any) {
    try {
      console.log({ values }, 'slogDATAsddasdsa');

      const payload: any = {
        ...values,
      };
      const result = await AddCmsContent.mutateAsync({ ...payload });
      toast({
        variant: 'success',
        title: 'Cms Content Add Successfully',
      });
      localStorage.removeItem('cmscontent');

      router.back();
    } catch (error: any) {
      toast({
        variant: 'destructive',
        title: error.message,
      });
    }
  }

  useEffect(() => {
    const title = form?.watch('slug');
    const altText = title
      ?.toLowerCase()
      .replaceAll(' ', '-')
      ?.replace(/[^a-zA-Z0-9_-]/g, '')
      .toLowerCase();
    form.setValue('slug', altText);
  }, [form?.watch('slug')]);

  // update data andler
  const onSubmitUpdate = async (values: any) => {
    try {
      const payload = {
        id: +id,
        // content: content,
        ...values,
      };
      const result = await updateCmsId.mutateAsync(payload);
      toast({
        variant: 'success',
        title: 'Cms Content Update Successfully',
      });
      localStorage.removeItem('cmscontent');

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
    if (contentEn || contentAr) {
      window.localStorage.setItem(
        'cmscontent',
        JSON.stringify({ en: contentEn, ar: contentAr }),
      );
      window.open('/admin/cms/preview', '_blank');
    } else {
      toast({
        variant: 'destructive',
        title: 'Please add source content to view preview.',
      });
    }
  };

  // Editor Config
  const editorConfig = {
    stylesSet: 'default',
    allowedContent: true,
    autoParagraph: false,
    extraAllowedContent: 'b i div class style;script[src]',
  };

  const FaqsType = [
    {
      faqType: 'event_faqs',
    },
    {
      faqType: 'static',
    },
  ];

  return (
    <Form {...form}>
      <form
        onSubmit={form.handleSubmit(id ? onSubmitUpdate : onSubmit)}
        className="space-y-4"
      >
        <div
          className={
            props.language.code === 'en'
              ? 'block lg:space-y-4 space-y-4 '
              : 'hidden lg:space-y-4 space-y-4'
          }
        >
          <div className="flex flex-col lg:flex-row md:flex-row justify-between  gap-2">
            <FormField
              control={form.control}
              name="slug"
              render={({ field }) => (
                <FormItem className="w-full">
                  <FormLabel>
                    Slug<sup className="text-md text-red-500">*</sup>
                  </FormLabel>
                  <FormControl>
                    <Input
                      placeholder="Enter Page Slug"
                      defaultValue={field.value}
                      {...field}
                    />
                  </FormControl>

                  <div className="relative pb-2 ">
                    <FormMessage />
                  </div>
                </FormItem>
              )}
            />
            <FormField
              control={form.control}
              name="type"
              render={({ field }) => (
                <FormItem className="w-full">
                  <FormLabel>
                    Cms Type<sup className="text-md text-red-500">*</sup>
                  </FormLabel>
                  <Select
                    onValueChange={field.onChange}
                    defaultValue={field.value}
                    value={field.value}
                  >
                    <FormControl className="rounded-md bg-[#060B0E]">
                      <SelectTrigger className=" rounded-none  ">
                        <SelectValue placeholder="Select Cms Type" />
                      </SelectTrigger>
                    </FormControl>
                    <SelectContent>
                      <SelectGroup>
                        {FaqsType?.map((item, i) => (
                          <SelectItem
                            className=" capitalize"
                            key={i}
                            value={item?.faqType}
                          >
                            {item?.faqType.replace('_', ' ')}
                          </SelectItem>
                        ))}
                      </SelectGroup>
                    </SelectContent>
                  </Select>

                  <div className="relative pb-2 mb-2">
                    <FormMessage />
                  </div>
                </FormItem>
              )}
            />
          </div>
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
              <div className="flex flex-col lg:flex-row md:flex-row justify-between gap-2 ">
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

                      <div className="relative pb-2 mb-2">
                        <FormMessage />
                      </div>
                    </FormItem>
                  )}
                />
                <FormField
                  control={form.control}
                  name="en.meta_keywords"
                  render={({ field }) => (
                    <FormItem className="w-full">
                      <FormLabel>
                        Meta Keywords{' '}
                        <sup className="text-md text-red-500">*</sup>
                      </FormLabel>
                      <FormControl>
                        <Input
                          placeholder="Enter Meta Keywords..."
                          {...field}
                        />
                      </FormControl>

                      <div className="relative pb-2 mb-2">
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
                  <FormItem className="w-full">
                    <FormLabel>
                      Description <sup className="text-md text-red-500">*</sup>
                    </FormLabel>
                    <FormControl>
                      <Textarea placeholder="Enter Description..." {...field} />
                    </FormControl>

                    <div className="relative pb-2 mb-2">
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
                    <FormLabel className=" text-white">
                      Content <sup className="text-md text-red-500">*</sup>
                    </FormLabel>
                    <FormControl>
                      <CKEditor
                        activeClass="p10"
                        content={contentEn} // Set the initial content
                        events={{
                          change: handleEnChange,
                        }}
                        config={editorConfig}
                      />
                    </FormControl>

                    <div className="relative pb-2 mb-2">
                      <FormMessage />
                    </div>
                  </FormItem>
                )}
              />
            </TabsContent>
            <TabsContent value="ar">
              <div dir="rtl">
                <div className="flex flex-col lg:flex-row md:flex-row justify-between gap-2 ">
                  <FormField
                    control={form.control}
                    name="ar.title"
                    render={({ field }) => (
                      <FormItem className="w-full">
                        <FormLabel>
                          عنوان<sup className="text-md text-red-500">*</sup>
                        </FormLabel>
                        <FormControl>
                          <Input placeholder="أدخل عنوان الصفحة" {...field} />
                        </FormControl>

                        <div className="relative pb-2 mb-2">
                          <FormMessage />
                        </div>
                      </FormItem>
                    )}
                  />
                  <FormField
                    control={form.control}
                    name="ar.meta_keywords"
                    render={({ field }) => (
                      <FormItem className="w-full">
                        <FormLabel>
                          ميتا الوصف{' '}
                          <sup className="text-md text-red-500">*</sup>
                        </FormLabel>
                        <FormControl>
                          <Input
                            placeholder="أدخل الوصف التعريفي..."
                            {...field}
                          />
                        </FormControl>

                        <div className="relative pb-2 mb-2">
                          <FormMessage />
                        </div>
                      </FormItem>
                    )}
                  />
                </div>
                <FormField
                  control={form.control}
                  name="ar.desc"
                  render={({ field }) => (
                    <FormItem>
                      <FormLabel>
                        وصف <sup className="text-md text-red-500">*</sup>
                      </FormLabel>
                      <FormControl>
                        <Textarea placeholder="أدخل الوصف..." {...field} />
                      </FormControl>

                      <div className="relative pb-2 mb-2">
                        <FormMessage />
                      </div>
                    </FormItem>
                  )}
                />

                <FormField
                  control={form.control}
                  name="ar.content"
                  render={({ field }) => (
                    <FormItem className=" text-black">
                      <FormLabel className=" text-white">
                        محتوى <sup className="text-md text-red-500">*</sup>
                      </FormLabel>
                      <FormControl>
                        <CKEditor
                          activeClass="p10"
                          content={contentAr} // Set the initial content
                          events={{
                            change: handleArChange,
                          }}
                          config={editorConfig}
                        />
                      </FormControl>

                      <div className="relative pb-2 mb-2">
                        <FormMessage />
                      </div>
                    </FormItem>
                  )}
                />
              </div>
            </TabsContent>
          </Tabs>
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
          open={
            AddCmsContent.isLoading ||
            loading ||
            updateCmsId.isLoading ||
            isFetching
          }
          text={`${id ? 'Updating' : 'Adding'} CMS...`}
        />
      </form>
    </Form>
  );
}
