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
import { Textarea } from '@/ui/textarea';
import { useForm } from 'react-hook-form';
import { useRouter } from 'next/router';

const exampleFormSchema = z.object({
  name: z
    .string()
    .min(2, {
      message: 'Name must be at least 2 characters.',
    })
    .max(24, {
      message: 'Name must not exceed 24 characters.',
    }),
  desc: z
    .string()
    .min(6, {
      message: 'Description must be at least 2 characters.',
    })
    .max(255, {
      message: 'Description must not exceed 255 characters.',
    }),
});

export default function CategoryForm() {
  // 1. Define your form.
  const form = useForm<z.infer<typeof exampleFormSchema>>({
    resolver: zodResolver(exampleFormSchema),
    defaultValues: {
      name: '',
      desc: '',
    },
  });

  // 2. Define a submit handler.
  function onSubmit(values: z.infer<typeof exampleFormSchema>) {
    // Do something with the form values.
    // ✅ This will be type-safe and validated.
    console.log(values);
  }

  return (
    <Form {...form}>
      <form onSubmit={form.handleSubmit(onSubmit)} className="space-y-2">
        <FormField
          control={form.control}
          name="name"
          render={({ field }) => (
            <FormItem>
              <FormLabel>Name</FormLabel>
              <FormControl>
                <Input placeholder="Enter Category Name" {...field} />
              </FormControl>
              <FormDescription>This is category name.</FormDescription>
              <FormMessage />
            </FormItem>
          )}
        />

        <FormField
          control={form.control}
          name="desc"
          render={({ field }) => (
            <FormItem>
              <FormLabel>Description</FormLabel>
              <FormControl>
                <Textarea placeholder="Enter Description" {...field} />
              </FormControl>
              <FormDescription>This is category description.</FormDescription>
              <FormMessage />
            </FormItem>
          )}
        />

        <Button type="submit" variant={'clip'}>
          Submit
        </Button>
      </form>
    </Form>
  );
}
