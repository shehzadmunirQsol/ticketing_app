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
import { FileInput } from '~/components/common/file_input';
import { useEffect, useState } from 'react';
import { trpc } from '~/utils/trpc';
import { getS3ImageUrl } from '~/service/api/s3Url.service';
import { compressImage, isValidImageType } from '~/utils/helper';
import { useToast } from '~/components/ui/use-toast';
// import { LoadingDialog } from '../../modal/loadingModal';
import { Editor } from 'primereact/editor';

//theme
import 'primereact/resources/themes/lara-light-indigo/theme.css';
//core
import 'primereact/resources/primereact.min.css';

export default function Faqs() {
  const { toast } = useToast();
  const router = useRouter();

  const form = useForm<z.infer<any>>();


  // register customer
  const cmsAboutUs = trpc.cms.cmsAboutUs.useMutation({
    onSuccess: (res: any) => {
     console.log(res,"REs")
    },
    onError: (err) => {
      console.log(err.message, 'err');
    },
  });

  // 1. Define your form.

  // 2. Define a submit handler.
  async function onSubmit(values: z.infer<any>) {
    try {
      const result = await cmsAboutUs.mutateAsync(values);
      console.log(result,"cmsAboutUs")
    } catch (error) {
      
    }
    console.log(values,'main');
  }

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
    <Form {...form}>
      <form
        onSubmit={form.handleSubmit(onSubmit)}
        className="justify-center items-center px-8 py-4 space-y-4"
      >
        <FormField
          control={form.control}
          name="detail"
          render={({ field }) => (
            <FormItem className=" bg-black">
              <FormLabel>About us</FormLabel>
              <FormControl>
                <div dir="ltr">
                  <Editor
                    id={field.name}
                    name="about"
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
        <div>
          <Button type="submit" variant={'clip'} className="w-1/2">
            {/* {index ? 'Edit Event' : 'Add Event'} */}
            Add
          </Button>
        </div>
      </form>
      {/* <LoadingDialog open={isSubmitting} text={'Saving data...'} /> */}
    </Form>
  );
}
