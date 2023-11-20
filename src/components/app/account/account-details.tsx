import React, { useState } from 'react';
import { Button } from '~/components/ui/button';
import {
  accountsDetailSchemaInput,
  passwordChangeSchemaInput,
  deleteMyAccountCustomerSchema,
  deleteMyAccountCustomerSchemaInput,
} from '~/schema/customer';
import {
  Form,
  FormControl,
  FormField,
  FormItem,
  FormLabel,
  FormMessage,
} from '@/ui/form';
import { useForm } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';
import { Input } from '~/components/ui/input';
import { Textarea } from '~/components/ui/textarea';
import { useDispatch, useSelector } from 'react-redux';
import { RootState } from '~/store/store';
import { trpc } from '~/utils/trpc';
import { useToast } from '~/components/ui/use-toast';
import { userAuth } from '~/store/reducers/auth';
import { LoadingDialog } from '~/components/common/modal/loadingModal';
import langContent from '~/locales';

const AccountDetails = () => {
  const { toast } = useToast();
  const { user } = useSelector((state: RootState) => state.auth);
  const { lang } = useSelector((state: RootState) => state.layout);

  const dispatch = useDispatch();

  // 1. Define your form.
  const form = useForm<accountsDetailSchemaInput>({
    resolver: zodResolver(accountsDetailSchemaInput),
    defaultValues: {
      first_name: user?.first_name,
      last_name: user?.last_name,
      dob: user?.dob?.toISOString()?.split('T')[0],
    },
  });

  const updateCustomerAccountDetail =
    trpc.customer.updateCustomerAccountDetail.useMutation({
      onSuccess: async (res: any) => {
        dispatch(userAuth(res?.user));

        toast({
          variant: 'success',
          title: 'Your Account Info Update Successfully ',
        });
      },
      onError: (err) => {
        console.log(err.message, 'err');
      },
    });

  // handle account detail
  async function onSubmitAccountDetail(values: accountsDetailSchemaInput) {
    try {
      const resp = await updateCustomerAccountDetail.mutateAsync({
        id: user.id,
        ...values,
      });
      dispatch(userAuth(resp?.user));
    } catch (error: any) {
      console.log({ error });
    }
  }
  const today = new Date();

  // Calculate the minimum date (18 years ago from today)
  const minDate = new Date(
    today.getFullYear() - 18,
    today.getMonth(),
    today.getDate(),
  );

  // Format the minimum date as "YYYY-MM-DD" for the input field
  const minDateFormatted = minDate.toISOString().split('T')[0];
  return (
    <div className="py-4 px-6 text-[#eaeaea] ">
      <p className=" font-bold text-2xl mb-6 text-white">
        {langContent[lang.lang].MyAccount.AccountDetail.HEADING}
      </p>
      <div dir="ltr" className="space-y-32 ">
        <Form {...form}>
          <form
            onSubmit={form.handleSubmit(onSubmitAccountDetail)}
            className="justify-center items-center"
          >
            <FormField
              control={form.control}
              name="first_name"
              render={({ field }) => (
                <FormItem className=" w-full ">
                  <FormLabel className="text-xs font-thin text-grayColor">
                    First Name*
                  </FormLabel>
                  <FormControl className="rounded-md bg-inputColor ">
                    <Input
                      type="text"
                      {...field}
                    />
                  </FormControl>
                  <div className="relative pb-2 errormsg">
                    <FormMessage />
                  </div>
                </FormItem>
              )}
            />
            <FormField
              control={form.control}
              name="last_name"
              render={({ field }) => (
                <FormItem className=" w-full ">
                  <FormLabel className="text-xs font-thin text-grayColor">
                    Last Name *
                  </FormLabel>
                  <FormControl className="rounded-md bg-inputColor ">
                    <Input
                      type="text"
                      {...field}
                    />
                  </FormControl>
                  <div className="relative pb-2 errormsg">
                    <FormMessage />
                  </div>
                </FormItem>
              )}
            />

            <div className="flex flex-col sm:flex-row justify-center items-center gap-2">
              <FormItem className=" w-full ">
                <FormLabel className="text-xs font-thin text-grayColor">
                  Email Address *
                </FormLabel>
                <FormControl className="rounded-md bg-inputColor ">
                  <Input
                    type="text"
                    disabled
                    defaultValue={user?.email ?? ''}
                  />
                </FormControl>
                <div className="relative pb-2 errormsg">
                  <FormMessage />
                </div>
              </FormItem>

              <FormField
                control={form.control}
                name="dob"
                render={() => (
                  <FormItem className=" flex flex-col w-full">
                    <FormLabel className="text-xs font-thin text-grayColor">
                      Date of Birth *
                    </FormLabel>
                    <FormControl className="rounded-md bg-inputColor">
                      <Input
                        type={'date'}
                        max={minDateFormatted}
                        {...form.register('dob', {
                          valueAsDate: true,
                        })}
                      />
                    </FormControl>
                    <div className="relative pb-2 errormsg">
                      <FormMessage />
                    </div>
                  </FormItem>
                )}
              />
            </div>

            <div className=" flex items-center ltr:justify-end rtl:justify-start">
              <Button
                className="align-center  rounded-full px-5 text-base   text-black font-sans font-[900]   tracking-[-1px]"
                variant="clip"
              >
                {langContent[lang.lang].MyAccount.AccountDetail.BUTTON}
              </Button>
            </div>
          </form>
        </Form>
      </div>
      <div>
        <hr className=" opacity-20 mt-4" />
      </div>
      <div>
        <PasswordChange email={user?.email} />
        <DeleteAccount email={user?.email} />
      </div>

      <LoadingDialog
        open={updateCustomerAccountDetail.isLoading}
        text="Saving data..."
      />
    </div>
  );
};
export default AccountDetails;

// Password Change
function PasswordChange({ email }: any) {
  const { lang } = useSelector((state: RootState) => state.layout);

  const { toast } = useToast();

  // 1. Define your form.
  const form = useForm<passwordChangeSchemaInput>({
    resolver: zodResolver(passwordChangeSchemaInput),
  });

  console.log(form?.getValues(), 'helloworld');
  // handle password update
  const updateCustomerPassword =
    trpc.customer.updateCustomerPassword.useMutation({
      onSuccess: async (res: any) => {
        console.log(res, 'updateCustomerPassword res');
        toast({
          variant: 'success',
          title: 'Your Account Password Updated Successfully ',
        });
        form.setValue('currentPassword', '');
        form.setValue('newPassword', '');
        form.setValue('confirmPassword', '');
      },
      onError: (err) => {
        console.log(err.message, 'err');
      },
    });

  // handle account detail
  async function onSubmitAccountPassword(values: any) {
    try {
      const payload: any = {
        email: email,
        ...values,
      };
      const resp = await updateCustomerPassword.mutateAsync(payload);
      console.log({ resp });
    } catch (e: any) {
      toast({
        variant: 'destructive',
        title: e.message,
      });
    }
  }

  return (
    <div className="py-4 text-[#eaeaea]">
      <p className=" font-bold text-2xl mb-6 text-white">
        {langContent[lang.lang].MyAccount.AccountDetail.SUB_HEADING}
      </p>
      <div dir="ltr">
        <Form {...form}>
          <form
            onSubmit={form.handleSubmit(onSubmitAccountPassword)}
            className="justify-center items-center"
          >
            <FormField
              control={form.control}
              name="currentPassword"
              render={({ field }) => (
                <FormItem className=" w-full ">
                  <FormLabel className="text-xs font-thin text-grayColor">
                    Current password (leave blank to leave unchanged) *
                  </FormLabel>
                  <FormControl className="rounded-md bg-inputColor ">
                    <Input
                      type="password"
                      {...field}
                    />
                  </FormControl>
                  <div className="relative pb-2 errormsg">
                    <FormMessage />
                  </div>
                </FormItem>
              )}
            />
            <FormField
              control={form.control}
              name="newPassword"
              render={({ field }) => (
                <FormItem className=" w-full ">
                  <FormLabel className="text-xs font-thin text-grayColor">
                    New password (leave blank to leave unchanged) *
                  </FormLabel>
                  <FormControl className="rounded-md bg-inputColor ">
                    <Input
                      type="password"
                      {...field}
                    />
                  </FormControl>
                  <div className="relative pb-2 errormsg">
                    <FormMessage />
                  </div>
                </FormItem>
              )}
            />
            <FormField
              control={form.control}
              name="confirmPassword"
              render={({ field }) => (
                <FormItem className=" w-full ">
                  <FormLabel className="text-xs font-thin text-grayColor">
                    Password Confirmation (leave blank to leave unchanged) *
                  </FormLabel>
                  <FormControl className="rounded-md bg-inputColor ">
                    <Input
                      type="password"
                      {...field}
                    />
                  </FormControl>
                  <div className="relative pb-2 errormsg">
                    <FormMessage />
                  </div>
                </FormItem>
              )}
            />
            <div className=" flex items-center ltr:justify-end rtl:justify-start">
              <Button
                className="align-center  rounded-full px-5 text-base   text-black font-sans font-[900]   tracking-[-1px]"
                variant="clip"
              >
                {langContent[lang.lang].MyAccount.AccountDetail.BUTTON}
              </Button>
            </div>
          </form>
        </Form>
      </div>
      <div>
        <hr className=" opacity-20 mt-4" />
      </div>

      <LoadingDialog
        open={updateCustomerPassword.isLoading}
        text="Saving data..."
      />
    </div>
  );
}

// Delete Account
function DeleteAccount({ email }: any) {
  const { toast } = useToast();
  const { user } = useSelector((state: RootState) => state.auth);
  const { lang } = useSelector((state: RootState) => state.layout);

  // 1. Define your form.
  const form = useForm<deleteMyAccountCustomerSchemaInput>({
    resolver: zodResolver(deleteMyAccountCustomerSchema),
  });
  const [reason, setReason] = useState<any>([]);
  const dispatch = useDispatch();

  const deleteAccountRequestCustomer =
    trpc.customer.deleteMyAccountCustomer.useMutation({
      onSuccess: async (res: any) => {
        console.log(res, 'updateCustomerAccountDetail res');
        dispatch(userAuth(res?.user));
        toast({
          variant: 'success',
          title: 'Your Account is Deleted Successfully ',
        });
      },
      onError: (err) => {
        console.log(err.message, 'err');
      },
    });

  // handle account detail
  async function onSubmitDeleteAccountRequest(values: any) {
    try {
      const payload: any = {
        email: email,
        reasons: reason,
        ...values,
      };
      const resp = await deleteAccountRequestCustomer.mutateAsync(payload);

      console.log(resp, 'values account');
    } catch (e: any) {
      toast({
        variant: 'destructive',
        title: e.message,
      });
    }
  }

  const handleDivClick = (itemText: string) => {
    setReason((previous: any) => {
      if (previous.includes(itemText)) {
        return previous.filter((reason: any) => reason !== itemText);
      } else {
        return [...previous, itemText];
      }
    });
  };

  return (
    <div className="py-4 px-6 text-[#eaeaea]">
      <p className=" font-bold text-2xl text-white">
        {langContent[lang.lang].MyAccount.AccountDetail.DELETE_HEADING}
      </p>
      <p className="text-grayColor text-sm">
        {langContent[lang.lang].MyAccount.AccountDetail.DELETE_INFO}
      </p>
      <div>
        <Form {...form}>
          <form
            onSubmit={form.handleSubmit(onSubmitDeleteAccountRequest)}
            className="justify-center items-center  py-4"
          >
            <div className="flex flex-row gap-2 justify-start w-full  items-center">
              <div>
                <Input
                  type="checkbox"
                  className="accent-white text-2xl "
                  disabled={user?.is_disabled}
                  required
                />
              </div>
              <p className="text-sm">
                {langContent[lang.lang].MyAccount.AccountDetail.UNDERSTAND}
              </p>
            </div>

            <p className="  text-xl text-white mb-5 mt-10 ">
              {langContent[lang.lang].MyAccount.AccountDetail.OPTIONAL}
            </p>

            {langContent[
              lang.lang
            ].MyAccount.AccountDetail.accountArgumentsOptions?.map(
              (item, i) => {
                return (
                  <div
                    className="flex flex-row gap-2 justify-start w-full  items-center cursor-pointer"
                    key={i}
                    onClick={() => handleDivClick(item.text)}
                  >
                    <div>
                      <Input
                        type="checkbox"
                        className="accent-white text-2xl "
                        checked={reason.includes(item?.text)}
                        disabled={user?.is_disabled}
                      />
                    </div>
                    <p className="text-lightTextColor text-sm">{item.text}</p>
                  </div>
                );
              },
            )}

            <FormField
              control={form.control}
              name="message"
              render={({ field }) => (
                <FormItem className="mb-6 lg:mb-10 md:mb-10 " dir="ltr">
                  <FormLabel className="text-xs  font-thin text-grayColor">
                    Add your comment:
                  </FormLabel>
                  <FormControl className="rounded-md bg-inputColor ">
                    <Textarea
                      {...field}
                      disabled={user?.is_disabled}
                    />
                  </FormControl>
                  <div className="relative pb-2 errormsg">
                    <FormMessage />
                  </div>
                </FormItem>
              )}
            />

            <div className=" flex items-center justify-end">
              <Button
                className="align-center  rounded-full px-5 text-base   text-black font-sans font-[900]   tracking-[-1px]"
                variant="clip"
                disabled={user?.is_disabled}
              >
                {langContent[lang.lang].MyAccount.AccountDetail.BUTTON_CONTENT}
              </Button>
            </div>
          </form>
        </Form>
      </div>

      <LoadingDialog
        open={deleteAccountRequestCustomer.isLoading}
        text="Saving data..."
      />
    </div>
  );
}
