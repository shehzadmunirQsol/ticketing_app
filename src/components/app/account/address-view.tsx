import React, { useState } from 'react';
import { useForm } from 'react-hook-form';
import { useSelector } from 'react-redux';
import { useToast } from '~/components/ui/use-toast';
import { RootState } from '~/store/store';
import { trpc } from '~/utils/trpc';
import { addAddressInput } from '~/schema/customer';
import { formatTrpcError } from '~/utils/helper';
import langContent from '~/locales';


const AddressesView = () => {
  const { toast } = useToast();
  const { user } = useSelector((state: RootState) => state.auth);
  const { lang } = useSelector((state: RootState) => state.layout);



  const { handleSubmit, register, setValue, getValues } =
    useForm<addAddressInput>();
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

  const handleChange = (e: any) => {
    e.preventDefault();
    console.log({ isEdit, action }, 'check states before', action.adding.text);
    setEdit(!isEdit);
  };
  const { data, isFetching, isRefetching, isFetched, isLoading, refetch } =
    trpc.customer.getAddress.useQuery(
      { customer_id: user?.id },
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
            console.log({ data });
            setValue('city', data.city);
            setValue('phone_number', data.phone_number);
            setValue('country', data.country);
            setValue('postal_code', data.postal_code);
            setValue('street_address_1', data.street_address_1);
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
      setEdit(false);
    },
    onError(error) {
      const errorMessage = formatTrpcError(error?.shape?.message);

      toast({
        variant: 'destructive',
        title: errorMessage,
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
      setEdit(false);
    },
    onError(error) {
      const errorMessage = formatTrpcError(error?.shape?.message);

      toast({
        variant: 'destructive',
        title: errorMessage,
      });
    },
  });

  async function onSubmit(values: any) {
    try {
      console.log({ values });
      const payload = {
        customer_id: user.id,
        postal_code: Number(+values.postal_code),
        ...values,
      };
      console.log(payload.postal_code, typeof payload.postal_code);
      console.log({ payload }, 'create payload');
      const response: any =
        data === null
          ? await createAddress.mutateAsync(payload)
          : await updateAddress.mutateAsync({
              id: data.id,
              customer_id: data.customer_id,
              ...payload,
            });

      if (response) {
        console.log({ response }, 'creatded');
        refetch();
      }
    } catch (error: any) {}
  }
  console.log({ data }, 'values');
  return (
    <div className="py-4 px-6 text-[#eaeaea]">
      <p className="text-[#808080] text-sm">
      {langContent[lang.lang].MyAccount.AddressView.INFO}
      </p>

      <form className="mt-4 flex ">
        <div
          className={`w-64   rounded-md border border-border-[1px] p-4 ${
            data ? 'border-primary' : 'border-[#808080]'
          }  flex flex-col`}
        >
          <div className="flex justify-between items-center">
            <p className="text-md font-bold tracking-tight lead">
            {langContent[lang.lang].MyAccount.AddressView.INFO_HEADING}
            </p>
            <button
              type={isEdit ? 'submit' : 'button'}
              disabled={isRefetching || isLoading}
              onClick={isEdit ? handleSubmit(onSubmit) : handleChange}
              className="px-2 py-1.5 bg-primary text-sm text-[#101417] font-extrabold tracking-tight leading-tight w-fit rounded-full flex items-center justify-center gap-1 hover:cursor-pointer hover:opacity-90"
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
                      {langContent[lang.lang].MyAccount.AddressView.SUB_INFO}

              </p>
            )}

            {!isRefetching &&
              (data && !isRefetching && !isEdit ? (
                <div className="h-fit ">
                  <p>
                    {data?.Customer.first_name} {data?.Customer.last_name}
                  </p>
                  <p>      {langContent[lang.lang].MyAccount.AddressView.BOX} {data.postal_code}</p>
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
                    value={
                      user.first_name && user.first_name + ' ' + user.last_name
                    }
                    placeholder="Name"
                    type="text"
                    required={true}
                  />
                  <input
                    className="bg-primary-foreground p-0.5 font-sans"
                    placeholder="P.O Box"
                    {...register('postal_code', {
                      valueAsNumber: true,
                    })}
                    type="number"
                    required={true}
                  />
                  <input
                    className="bg-primary-foreground p-0.5 font-sans"
                    placeholder="Street Address"
                    {...register('street_address_1')}
                    type="text"
                    required={true}
                  />
                  <input
                    className="bg-primary-foreground p-0.5 font-sans"
                    {...register('phone_number')}
                    placeholder="Mobile"
                    type="text"
                    maxLength={9}
                    required={true}
                  />
                  <input
                    className="bg-primary-foreground p-0.5 font-sans"
                    placeholder="City"
                    {...register('city')}
                    type="text"
                    required={true}
                  />
                  <input
                    className="bg-primary-foreground p-0.5 font-sans"
                    placeholder="Country"
                    {...register('country')}
                    type="text"
                    required={true}
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
