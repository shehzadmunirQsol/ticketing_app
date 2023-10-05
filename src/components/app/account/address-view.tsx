import React, { useState } from 'react';
import { useSelector } from 'react-redux';
import { useToast } from '~/components/ui/use-toast';
import { RootState } from '~/store/store';
import { trpc } from '~/utils/trpc';
import { formatTrpcError } from '~/utils/helper';
import langContent from '~/locales';
import { Button } from '~/components/ui/button';
import { AddCustomerAddressDialog } from '~/components/common/modal/customerAddressModal';
import { LoadingDialog } from '~/components/common/modal/loadingModal';

export default function AddressesView() {
  const [isModal, setIsModal] = useState(false);
  const { lang } = useSelector((state: RootState) => state.layout);
  const { user } = useSelector((state: RootState) => state.auth);

  const {
    data: addresses,
    refetch,
    isLoading,
  } = trpc.customer.getAddress.useQuery(
    {
      customer_id: user?.id,
    },
    {
      refetchOnWindowFocus: false,
    },
  );

  function openChangeHandler() {
    setIsModal((prevState) => !prevState);
  }

  const usedAddress = addresses?.map((address) => address?.address_type);
  const availableAddressTypes = addressType.filter(
    (address) => !usedAddress?.includes(address?.value as any),
  );

  return (
    <div className="py-4 sm:px-6 space-y-4">
      <p className="text-[#808080] text-sm">
        {langContent[lang.lang].MyAccount.AddressView.INFO}
      </p>
      <div className="grid gap-4 items-start grid-cols-1 sm:grid-cols-2 lg:grid-cols-3">
        {addresses?.map((address) => (
          <CustomerAddress key={address.id} {...address} refetch={refetch} />
        ))}

        {addresses && addresses?.length < 4 ? (
          <div className="h-48 max-w-[256px] rounded-lg grid items-center justify-center border border-primary">
            <Button onClick={openChangeHandler} variant={'rounded'}>
              Add Another
            </Button>
          </div>
        ) : (
          ''
        )}
      </div>

      <AddCustomerAddressDialog
        isModal={isModal}
        openChangeHandler={openChangeHandler}
        customer_id={user?.id ?? 0}
        refetch={refetch}
        availableAddressTypes={availableAddressTypes}
      />

      <LoadingDialog open={isLoading} text="Loading..." />
    </div>
  );
}

type CustomerAddressType = {
  address_type: 'home' | 'work' | 'hotel' | 'other';
  id: number;
  refetch: () => void;
  customer_id: number;
  street_address_1: string | null;
  street_address_2: string | null;
  country: string | null;
  state: string | null;
  city: string | null;
  phone_number: string | null;
  phone_code: string | null;
  postal_code: number | null;
};

function CustomerAddress(props: CustomerAddressType) {
  const { toast } = useToast();
  const { lang } = useSelector((state: RootState) => state.layout);

  const [formValues, setFormValues] = useState({
    city: props.city ?? '',
    country: props.country ?? '',
    address_type: props.address_type ?? '',
    customer_id: props.customer_id,
    id: props.id,
    phone_number: props.phone_number ?? '',
    phone_code: props.phone_code ?? '',
    postal_code: props.postal_code ?? 0,
    street_address_1: props.street_address_1 ?? '0',
  });

  const [isEdit, setEdit] = useState(false);

  const handleChange = () => {
    setEdit(!isEdit);
  };

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

  async function onSubmit() {
    try {
      await updateAddress.mutateAsync({
        ...formValues,
        postal_code: +formValues.postal_code,
      });
    } catch (error: any) {
      console.log({ error });
    }
  }

  function formChangeHandler(
    event: React.ChangeEvent<HTMLInputElement | HTMLSelectElement>,
  ) {
    const { name, value } = event.target;
    setFormValues((preValues) => ({ ...preValues, [name]: value }));
  }

  const currentAddress = addressType.find(
    (address) => address.value === props.address_type,
  );

  return (
    <form className="flex mx-auto sm:mx-0">
      <div
        className={`w-64 rounded-md border border-border-[1px] p-4  border-primary
        flex flex-col`}
      >
        <div className="flex justify-between items-center">
          <p className="text-md font-bold tracking-tight lead">
            {currentAddress?.label}
            {/* {langContent[lang.lang].MyAccount.AddressView.INFO_HEADING} */}
          </p>
          <button
            type={'button'}
            onClick={isEdit ? onSubmit : handleChange}
            className="px-2 py-1.5 bg-primary text-sm text-[#101417] font-extrabold tracking-tight leading-tight w-fit rounded-full flex items-center justify-center gap-1 hover:cursor-pointer hover:opacity-90"
          >
            {!isEdit ? (
              <span>
                <i className={`fas fa-pencil`}></i>
              </span>
            ) : (
              ''
            )}
            <p className="text-xs">
              {isEdit
                ? langContent[lang.lang].MyAccount.AddressView.SAVE_BUTTON
                : langContent[lang.lang].MyAccount.AddressView.EDIT_BUTTON}
            </p>
          </button>
        </div>

        <div className="w-full h-full font-light text-[#eaeaea] flex flex-col pt-3 text-sm">
          <div className="space-y-1" dir="ltr">
            <p>
              {' '}
              {langContent[lang.lang].MyAccount.AddressView.BOX}{' '}
              {formValues.postal_code}
            </p>
            <p>
              {formValues?.phone_code}
              {formValues?.phone_number}
            </p>
            <p>{formValues?.country}</p>
            <p>{formValues?.city}</p>
            <p>{formValues?.street_address_1}</p>
          </div>
        </div>
      </div>
      <LoadingDialog open={updateAddress.isLoading} text="Updating..." />
      <AddCustomerAddressDialog
        isModal={isEdit}
        openChangeHandler={handleChange}
        availableAddressTypes={addressType}
        {...props}
      />
    </form>
  );
}

const addressType = [
  {
    label: 'Home',
    value: 'home',
  },
  {
    label: 'Work',
    value: 'work',
  },
  {
    label: 'Hotel',
    value: 'hotel',
  },
  {
    label: 'Other',
    value: 'other',
  },
];

// ) : (
//   // <div className="h-fit " dir="ltr">
//   //   <input
//   //     className="bg-primary-foreground p-0.5 font-sans"
//   //     placeholder="P.O Box"
//   //     name="postal_code"
//   //     value={formValues.postal_code}
//   //     onChange={formChangeHandler}
//   //     type="number"
//   //     required={true}
//   //   />
//   //   <div className="flex items-center gap-2">
//   //     <input
//   //       className="w-10 bg-primary-foreground p-0.5 font-sans"
//   //       name="phone_code"
//   //       value={formValues.phone_code}
//   //       onChange={formChangeHandler}
//   //       placeholder="+971"
//   //       type="text"
//   //       maxLength={5}
//   //       required={true}
//   //     />
//   //     <input
//   //       className="w-[104px] bg-primary-foreground p-0.5 font-sans"
//   //       name="phone_number"
//   //       value={formValues.phone_number}
//   //       onChange={formChangeHandler}
//   //       placeholder="Mobile"
//   //       type="text"
//   //       maxLength={9}
//   //       required={true}
//   //     />
//   //   </div>

//   //   <select
//   //     name="country"
//   //     value={formValues.country}
//   //     onChange={formChangeHandler}
//   //     className="w-[153px] overflow-hidden"
//   //   >
//   //     <option className="w-[153px] overflow-hidden" value="">
//   //       Select Country
//   //     </option>
//   //     {countries.map((country) => (
//   //       <option
//   //         className="w-[153px] overflow-hidden"
//   //         key={country}
//   //         value={country}
//   //       >
//   //         {country?.toUpperCase()?.substring(0, 16)}
//   //       </option>
//   //     ))}
//   //   </select>

//   //   <input
//   //     className="bg-primary-foreground p-0.5 font-sans"
//   //     placeholder="City"
//   //     name="city"
//   //     value={formValues.city}
//   //     onChange={formChangeHandler}
//   //     type="text"
//   //     required={true}
//   //   />
//   //   <input
//   //     className="bg-primary-foreground p-0.5 font-sans"
//   //     placeholder="Street Address"
//   //     name="street_address_1"
//   //     value={formValues.street_address_1}
//   //     onChange={formChangeHandler}
//   //     type="text"
//   //     required={true}
//   //   />
//   // </div>
// )}
