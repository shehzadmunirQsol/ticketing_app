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
import { trpc } from '~/utils/trpc';
import { setAdminToken } from '~/utils/authToken';
import { formatTrpcError } from '~/utils/helper';
import { useDispatch } from 'react-redux';
import {
  userAdminAuth,
  userAdminIsLogin,
} from '~/store/reducers/adminAuthSlice';
import toast from 'react-hot-toast';
import { LoadingDialog } from '../modal/loadingModal';

const exampleFormSchema = z.object({
  username: z
    .string()
    .min(2, {
      message: 'Username must be at least 2 characters.',
    })
    .trim(),
});

export default function ExampleForm() {
  // 1. Define your form.
  const form = useForm<z.infer<typeof exampleFormSchema>>({
    resolver: zodResolver(exampleFormSchema),
    defaultValues: {
      username: '',
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
      <form onSubmit={form.handleSubmit(onSubmit)} className="space-y-4 w-full">
        <FormField
          control={form.control}
          name="username"
          render={({ field }) => (
            <FormItem>
              <FormLabel>Username</FormLabel>
              <FormControl>
                <Input placeholder="Enter Username" {...field} />
              </FormControl>
              <FormDescription>
                This is your public display name.
              </FormDescription>

              <div className="relative pb-4">
                <FormMessage />
              </div>
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

const loginFormSchema = z.object({
  email: z.string().trim().email(),
  password: z
    .string()
    .min(6, {
      message: 'Password must be at least 6 characters.',
    })
    .trim(),
});

export function LoginForm() {
  const router = useRouter();
  const dispatch = useDispatch();
  const form = useForm<z.infer<typeof loginFormSchema>>({
    resolver: zodResolver(loginFormSchema),
    defaultValues: {
      email: '',
      password: '',
    },
  });

  const loginUser = trpc.admin.login.useMutation({
    onSuccess: (res: any) => {
      if (res.jwt) {
        setAdminToken(res.jwt);
        dispatch(userAdminAuth(res?.user));
        dispatch(userAdminIsLogin(true));
      }
    },
    onError(error) {
      console.log(error.message, 'ERROR');
    },
  });
  // 2. Define a submit handler.
  async function onSubmit(values: z.infer<typeof loginFormSchema>) {
    try {
      const response: any = await loginUser.mutateAsync({ ...values });

      toast.success('Login Successfully!');

      router.replace('/admin/dashboard');
    } catch (error: any) {
      const errorMessage = formatTrpcError(error?.shape?.message);

      toast.error(errorMessage);
    }
  }

  return (
    <Form {...form}>
      <form onSubmit={form.handleSubmit(onSubmit)} className="space-y-8">
        <div className="space-y-4">
          <FormField
            control={form.control}
            name="email"
            render={({ field }) => (
              <FormItem>
                <FormLabel>Email</FormLabel>
                <FormControl>
                  <Input type="email" placeholder="Enter Email" {...field} />
                </FormControl>

                <div className="relative pb-4">
                  <FormMessage />
                </div>
              </FormItem>
            )}
          />
          <FormField
            control={form.control}
            name="password"
            render={({ field }) => (
              <FormItem>
                <FormLabel>Password</FormLabel>
                <FormControl>
                  <Input
                    type="password"
                    placeholder="Enter password"
                    {...field}
                  />
                </FormControl>

                <div className="relative pb-4">
                  <FormMessage />
                </div>
              </FormItem>
            )}
          />
        </div>

        <Button type="submit" size={'full'}>
          Submit
        </Button>
      </form>
      <LoadingDialog open={loginUser.isLoading} text={'Loading...'} />
    </Form>
  );
}
