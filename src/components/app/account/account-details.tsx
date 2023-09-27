import React, { useState } from 'react';
import { Button } from '~/components/ui/button';
import {
  accountsDetailSchemaInput,
  passwordChangeSchema,
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

const AccountDetails = () => {
  const { toast } = useToast();
  const { user } = useSelector((state: RootState) => state.auth);
  const dispatch = useDispatch();

  // 1. Define your form.
  const form = useForm<accountsDetailSchemaInput>({
    resolver: zodResolver(accountsDetailSchemaInput),
    defaultValues: {
      email: user?.email,
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
      const resp = await updateCustomerAccountDetail.mutateAsync(values);
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
        Personal information
      </p>
      <div className="space-y-32">
        <Form {...form}>
          <form
            onSubmit={form.handleSubmit(onSubmitAccountDetail)}
            className="justify-center items-center  space-y-4"
          >
            <FormField
              control={form.control}
              name="first_name"
              render={({ field }) => (
                <FormItem className=" w-full ">
                  <FormLabel className="text-xs font-thin text-grayColor">
                    Name*
                  </FormLabel>
                  <FormControl className="rounded-md bg-inputColor">
                    <Input
                      type="text"
                      placeholder="Enter your name"
                      {...field}
                    />
                  </FormControl>
                  <div className="relative pb-2">
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
                    Last Name*
                  </FormLabel>
                  <FormControl className="rounded-md bg-inputColor">
                    <Input
                      type="text"
                      placeholder="Enter your last name"
                      {...field}
                    />
                  </FormControl>
                  <div className="relative pb-2">
                    <FormMessage />
                  </div>
                </FormItem>
              )}
            />
            <FormField
              control={form.control}
              name="email"
              render={({ field }) => (
                <FormItem className=" w-full ">
                  <FormLabel className="text-xs font-thin text-grayColor">
                    Email Address*
                  </FormLabel>
                  <FormControl className="rounded-md bg-inputColor">
                    <Input
                      type="text"
                      disabled
                      placeholder="Enter your email address"
                      {...field}
                    />
                  </FormControl>
                  <div className="relative pb-2">
                    <FormMessage />
                  </div>
                </FormItem>
              )}
            />
            <FormField
              control={form.control}
              name="dob"
              render={() => (
                <FormItem className=" flex flex-col gap-2 mt-2 w-full">
                  <FormLabel>
                    Date of Birth*
                    {/* <sup className="text-md text-red-500">*</sup> */}
                  </FormLabel>
                  <FormControl className="rounded-md bg-inputColor">
                    <Input
                      type={'date'}
                      placeholder={'Start Date'}
                      max={minDateFormatted}
                      {...form.register('dob', {
                        valueAsDate: true,
                      })}
                    />
                  </FormControl>
                  <div className="relative pb-2">
                    <FormMessage />
                  </div>
                </FormItem>
              )}
            />

            <div className=" flex items-center justify-end">
              <Button
                className="align-center  rounded-full px-5 text-base   text-black font-sans font-[900]   tracking-[-1px]"
                variant="clip"
              >
                SAVE CHANGES
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
  const { toast } = useToast();

  // 1. Define your form.
  const form = useForm<passwordChangeSchemaInput>({
    resolver: zodResolver(passwordChangeSchema),
  });

  // handle password update
  const updateCustomerPassword =
    trpc.customer.updateCustomerPassword.useMutation({
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
      <p className=" font-bold text-2xl mb-6 text-white">Password change</p>
      <div>
        <Form {...form}>
          <form
            onSubmit={form.handleSubmit(onSubmitAccountPassword)}
            className="justify-center items-center space-y-4"
          >
            <FormField
              control={form.control}
              name="currentPassword"
              render={({ field }) => (
                <FormItem className=" w-full ">
                  <FormLabel className="text-xs font-thin text-grayColor">
                    Current password (leave blank to leave unchanged) *
                  </FormLabel>
                  <FormControl className="rounded-md bg-inputColor">
                    <Input
                      type="password"
                      placeholder="Write your password"
                      {...field}
                    />
                  </FormControl>
                  <div className="relative pb-2">
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
                  <FormControl className="rounded-md bg-inputColor">
                    <Input
                      type="password"
                      placeholder="Enter your new password"
                      {...field}
                    />
                  </FormControl>
                  <div className="relative pb-2">
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
                  <FormControl className="rounded-md bg-inputColor">
                    <Input
                      type="password"
                      placeholder="Reenter your new password"
                      {...field}
                    />
                  </FormControl>
                  <div className="relative pb-2">
                    <FormMessage />
                  </div>
                </FormItem>
              )}
            />
            <div className=" flex items-center justify-end">
              <Button
                className="align-center  rounded-full px-5 text-base   text-black font-sans font-[900]   tracking-[-1px]"
                variant="clip"
              >
                SAVE CHANGES
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
// Password Change
function DeleteAccount({ email }: any) {
  const { toast } = useToast();
  const { user } = useSelector((state: RootState) => state.auth);

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

  const accountArgumentsOptions = [
    {
      text: 'I’m not interested in competitions anymore',
    },
    {
      text: 'Uninteresting prizes',
    },
    {
      text: 'Problems with gambling',
    },
    {
      text: 'I receive too many emails',
    },
    {
      text: 'I receive too many text messages',
    },
    {
      text: 'I receive too many push notifications',
    },
  ];

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
        Delete account & personal details
      </p>
      <p className="text-grayColor text-sm">
        If you want to delete your account and all personal details, we will
        start a process to delete your account. You won’t be able to use your
        account. This process takes 14 days and after that time, we won’t be
        able to recover your data.
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
                I understand that I will lose all data (including tickets)
                related to my account
              </p>
            </div>

            <p className="  text-xl text-white mb-5 mt-10 ">
              Why are you deleting your account? (optional)
            </p>

            {accountArgumentsOptions?.map((item, i) => {
              return (
                <div
                  className="flex flex-row gap-2 justify-start w-full  items-center cursor-pointer"
                  key={i}
                  // onChange={(e:any)=>setReason((previous:any)=>[...previous,item?.text])}
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
            })}

            <FormField
              control={form.control}
              name="message"
              render={({ field }) => (
                <FormItem className="mb-6 lg:mb-10 md:mb-10">
                  <FormLabel className="text-xs  font-thin text-grayColor">
                    Add your comment:
                  </FormLabel>
                  <FormControl className="rounded-md bg-inputColor">
                    <Textarea
                      placeholder="Type your message here..."
                      {...field}
                      disabled={user?.is_disabled}
                    />
                  </FormControl>
                  <div className="relative pb-2">
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
                DELETE MY ACCOUNT
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
