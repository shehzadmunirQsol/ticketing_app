import React from 'react';
import { Button } from '~/components/ui/button';
import {
  accountsDetailSchema,
  accountsDetailSchemaInput,
  passwordChangeSchema,
  passwordChangeSchemaInput,
} from '~/schema/customer';

import {
  Form,
  FormControl,
  FormDescription,
  FormField,
  FormItem,
  FormLabel,
  FormMessage,
} from '@/ui/form';
import { useForm } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';
import { Input } from '~/components/ui/input';
import { Textarea } from '~/components/ui/textarea';

const AccountDetails = () => {
  // 1. Define your form.
  const form = useForm<accountsDetailSchemaInput>({
    resolver: zodResolver(accountsDetailSchema),
  });

  // handle account detail
  async function onSubmitAccountDetail(values: any) {
    console.log(values, 'values account');
  }

  return (
    <div className="py-4 px-6 text-[#eaeaea] ">
      <p className=" font-bold text-2xl text-white">Personal information</p>
      <div className="space-y-32">
        <Form {...form}>
          <form
            onSubmit={form.handleSubmit(onSubmitAccountDetail)}
            className="justify-center items-center  py-4 space-y-10"
          >
            <FormField
              control={form.control}
              name="name"
              render={({ field }) => (
                <FormItem className=" w-full ">
                  <FormLabel className="text-xs font-thin text-grayColor">
                    Name
                  </FormLabel>
                  <FormControl className="rounded-md bg-inputColor">
                    <Input
                      type="text"
                      placeholder="Enter your name"
                      {...field}
                    />
                  </FormControl>
                  <FormMessage />
                </FormItem>
              )}
            />
            <FormField
              control={form.control}
              name="lastname"
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
                  <FormMessage />
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
                      placeholder="Enter your email address"
                      {...field}
                    />
                  </FormControl>
                  <FormMessage />
                </FormItem>
              )}
            />
            <FormField
              control={form.control}
              name="dateofbirth"
              render={({ field }) => (
                <FormItem className="w-full">
                  <FormLabel className="text-xs font-thin text-grayColor">
                    Date of Birth
                  </FormLabel>
                  <FormControl className="rounded-md bg-inputColor">
                    <Input
                      type="date"
                      placeholder="Enter your email address"
                      {...field}
                    />
                  </FormControl>
                  <FormMessage />
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
        <PasswordChange />
        <DeleteAccount />
      </div>
    </div>
  );
};

export default AccountDetails;

// Password Change
function PasswordChange() {
  // 1. Define your form.
  const form = useForm<passwordChangeSchemaInput>({
    resolver: zodResolver(passwordChangeSchema),
  });

  // handle account detail
  async function onSubmitAccountPassword(values: any) {
    console.log(values, 'values account');
  }

  return (
    <div className="py-4 px-6 text-[#eaeaea]">
      <p className=" font-bold text-2xl text-white">Personal information</p>
      <div>
        <Form {...form}>
          <form
            onSubmit={form.handleSubmit(onSubmitAccountPassword)}
            className="justify-center items-center  py-4 space-y-10"
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
                      type="text"
                      placeholder="Write your password"
                      {...field}
                    />
                  </FormControl>
                  <FormMessage />
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
                      type="text"
                      placeholder="Enter your new password"
                      {...field}
                    />
                  </FormControl>
                  <FormMessage />
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
                      type="text"
                      placeholder="Reenter your new password"
                      {...field}
                    />
                  </FormControl>
                  <FormMessage />
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
    </div>
  );
}

// Delete Account
// Password Change
function DeleteAccount() {
  // 1. Define your form.
  const form = useForm<passwordChangeSchemaInput>({
    resolver: zodResolver(passwordChangeSchema),
  });

  // handle account detail
  async function onSubmitAccountPassword(values: any) {
    console.log(values, 'values account');
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
            onSubmit={form.handleSubmit(onSubmitAccountPassword)}
            className="justify-center items-center  py-4"
          >
            <div className="flex flex-row gap-2 justify-start w-full  items-center">
              <div>
                <Input type="checkbox" className="accent-white text-2xl " />
              </div>
              <p className="text-lightTextColor text-sm">
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
                  className="flex flex-row gap-2 justify-start w-full  items-center"
                  key={i}
                >
                  <div>
                    <Input type="checkbox" className="accent-white text-2xl " />
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
                    />
                  </FormControl>
                  <FormMessage />
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
    </div>
  );
}
