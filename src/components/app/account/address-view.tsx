import React, { useState } from 'react';
import { useForm } from 'react-hook-form';
import { useSelector } from 'react-redux';
import { useToast } from '~/components/ui/use-toast';
import { RootState } from '~/store/store';
import { trpc } from '~/utils/trpc';

const AddressesView = () => {
  const { toast } = useToast();
  const { user } = useSelector((state: RootState) => state.auth);
  const { handleSubmit, register, setValue, getValues } = useForm<any>();
  console.log({ user }, 'user');
  const [isEdit, setEdit] = useState(false);

  const [action, setAction] = useState({
    adding: {
      text: 'ADD',
      icon: 'fa-plus',
    },
    save: {
      text: 'SAVE',
      icon: 'fa-plus',
    },
  });

  const handleChange = (e) => {
    e.preventDefault();
    console.log({ isEdit, action }, 'check states before', action.adding.text);
    setEdit(!isEdit);
  };
  const { data, isFetching, isRefetching, isFetched, isLoading, refetch } =
    trpc.customer.getAddress.useQuery(
      { customer_id: user.id },
      {
        refetchOnWindowFocus: false,
        onSuccess: (data) => {
          if (data) {
            setAction({
              ...action,
              adding: {
                text: 'EDIT',
                icon: 'fa-pencil',
              },
            });
          }
        },
      },
    );

  const createAddress = trpc.customer.addAddress.useMutation({
    onSuccess: (res) => {
      console.log(res);
      toast({
        variant: 'success',
        title: 'Address Added Successfully',
      });
    },
    onError(error) {
      console.log(error);
      toast({
        variant: 'destructive',
        title: error.message,
      });
    },
  });
  const updateAddress = trpc.customer.updateAddress.useMutation({
    onSuccess: (res) => {
      console.log(res);
      toast({
        variant: 'success',
        title: 'Address Updated Successfully',
      });
    },
    onError(error) {
      console.log(error);
      toast({
        variant: 'destructive',
        title: error.message,
      });
    },
  });

  async function onSubmit(values) {
    console.log({ values }, 'values');

    try {
      const payload = {
        ...values,
        customer_id: user.id,
        name: user.first_name + ' ' + user.last_name,
      };
      console.log({payload},"create payload")
      const response: any = await createAddress.mutateAsync(payload);
      // : await updateAdminUser.mutateAsync({ id: props.id, ...payload });
    } catch (error: any) {}
  }
  console.log({ isEdit, action }, 'check states after', action.adding.text);

  return (
    <div className="py-4 px-6 text-[#eaeaea]">
      <p className="text-[#808080] text-sm">
        The following addresses will be used on the checkout page by default.
      </p>

      <form className="mt-4 flex ">
        <div
          className={`w-64   rounded-md border-[1px] p-4 ${
            data ? 'border-primary' : 'border-[#808080]'
          }  flex flex-col`}
        >
          <div className="flex justify-between items-center">
            <p className="text-md font-bold tracking-tight lead">
              Billing Address
            </p>
            <button
              type={isEdit ? 'submit' : 'button'}
              disabled={isRefetching || isLoading}
              onClick={isEdit ? handleSubmit(onSubmit) : handleChange}
              className="px-2 py-1.5 bg-primary text-sm text-[#101417] font-extrabold tracking-tight leading-tight w-fit rounded-full flex justify-center gap-1 hover:cursor-pointer hover:opacity-90"
            >
              <span>
                <i
                  className={`fas ${
                    isEdit ? '' + action.save.icon : '' + action.adding.icon
                  }`}
                ></i>
              </span>
              <p className="text-xs">
                {isEdit ? action.save.text : action.adding.text}
              </p>
            </button>
          </div>

          <div className="w-full h-full font-light text-[#eaeaea] flex flex-col pt-3 text-sm">
            {(isRefetching || isLoading) && (
              <>
                <div className="h-24 flex justify-center items-center">
                  <i className="fa-solid fa-circle-notch transition-all animate-spin text-lg  "></i>
                </div>
              </>
            )}

            {!isRefetching && !isEdit && data === null && (
              <p className=" h-24">
                You have not set up this type of address yet.
              </p>
            )}

            {!isRefetching &&
              (data && !isRefetching && !isEdit ? (
                <div className="h-fit ">
                  <p>
                    {data?.Customer.first_name} {data?.Customer.last_name}
                  </p>
                  <p>P.O Box {data.postal_code}</p>
                  <p>{data?.street_address_1}</p>
                  <p>{data?.phone_number}</p>
                  <p>
                    {data?.city} {data?.country}
                  </p>
                </div>
              ) : data || isEdit ? (
                <div className="h-fit ">
                  <input
                    className="bg-primary-foreground p-0.5 font-sans  "
                    value={user.first_name + ' ' + user.last_name}
                    placeholder="Name"
                    type="text"
                    readOnly
                  />
                  <input
                    className="bg-primary-foreground p-0.5 font-sans"
                    defaultValue={data?.postal_code}
                    placeholder="P.O Box"
                    {...register('postal_code')}
                    type="number"
                  />
                  <input
                    className="bg-primary-foreground p-0.5 font-sans"
                    defaultValue={data?.street_address_1}
                    placeholder="Street Address"
                    {...register('street_address_1')}
                    type="text"
                  />
                  <input
                    className="bg-primary-foreground p-0.5 font-sans"
                    defaultValue={data?.phone_number}
                    {...register('phone_number')}
                    placeholder="Mobile"
                    type="number"
                    prefix="05"
                    maxLength={9}
                  />
                  <input
                    className="bg-primary-foreground p-0.5 font-sans"
                    defaultValue={data?.city}
                    placeholder="City"
                    {...register('city')}
                    type="text"
                  />
                  <input
                    className="bg-primary-foreground p-0.5 font-sans"
                    defaultValue={data?.country}
                    placeholder="Country"
                    {...register('country')}
                    type="text"
                  />
                </div>
              ) : (
                ''
              ))}
          </div>
        </div>
      </form>
    </div>
  );
};

export default AddressesView;
