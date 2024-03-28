import { zodResolver } from '@hookform/resolvers/zod';
import { useEffect } from 'react';
import { useForm } from 'react-hook-form';
import { useSelector } from 'react-redux';
import { LoadingDialog } from '~/components/common/modal/loadingModal';
import { Button } from '~/components/ui/button';
import {
  Form,
  FormControl,
  FormField,
  FormItem,
  FormLabel,
  FormMessage,
} from '~/components/ui/form';
import { Input } from '~/components/ui/input';
import { useToast } from '~/components/ui/use-toast';
import { emailChangeSchema, emailChangeSchemaInput } from '~/schema/adminUser';
import { RootState } from '~/store/store';
import { trpc } from '~/utils/trpc';

function EmailChange() {
  const { user } = useSelector((state: RootState) => state.adminAuth);

  const { toast } = useToast();
  console.log({ user });
  // 1. Define your form.
  const form = useForm<emailChangeSchemaInput>({
    resolver: zodResolver(emailChangeSchema),
    defaultValues: {
      email: user?.email ?? '',
    },
  });
  useEffect(() => {
    if (user) {
      form.setValue('email', (user?.email as string) ?? '');
    }
  }, [user]);

  console.log(form?.getValues(), 'helloworld');
  // handle email update
  const updateCustomerEmail = trpc.admin.updateCustomerEmail.useMutation({
    onSuccess: async (res: any) => {
      console.log(res, 'updateCustomerPassword res');
      toast({
        variant: 'success',
        title: 'Your Account Password Updated Successfully ',
      });
    },
    onError: (err) => {
      console.log(err.message, 'err');
    },
  });

  // handle account detail
  async function onSubmitAccountPassword(values: any) {
    try {
      const payload: any = {
        id: user?.id,
        ...values,
      };
      const resp = await updateCustomerEmail.mutateAsync(payload);
      console.log({ resp });
    } catch (e: any) {
      toast({
        variant: 'destructive',
        title: e.message,
      });
    }
  }
  console.log(form.formState.errors, 'errors');

  return (
    <div className="py-4 text-[#eaeaea] border px-2 rounded-md border-border ">
      <p className=" font-bold text-2xl mb-6 text-gray-700">Update Email</p>
      <div dir="ltr">
        <Form {...form}>
          <form
            onSubmit={form.handleSubmit(onSubmitAccountPassword)}
            className="justify-center items-center"
          >
            <FormField
              control={form.control}
              name="email"
              render={({ field }) => (
                <FormItem className=" w-full ">
                  <FormLabel className="text-xs font-thin text-gray-600">
                    Email *
                  </FormLabel>
                  <FormControl className="rounded-md bg-inputColor text-gray-600">
                    <Input type="email" {...field} />
                  </FormControl>
                  <div className="relative pb-2 errormsg">
                    <FormMessage />
                  </div>
                </FormItem>
              )}
            />

            <div className=" flex items-center ltr:justify-end rtl:justify-start">
              <Button
                className="align-center  rounded-full px-5 text-base   text-white font-sans font-[700]   "
                variant="clip"
              >
                Update Email
              </Button>
            </div>
          </form>
        </Form>
      </div>
      <div></div>

      <LoadingDialog
        open={updateCustomerEmail.isLoading}
        text="Saving data..."
      />
    </div>
  );
}
export default EmailChange;
