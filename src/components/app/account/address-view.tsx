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

import countryJSON from '~/data/countries.json';
import {
  DropdownMenu,
  DropdownMenuContent,
  DropdownMenuItem,
  DropdownMenuSeparator,
  DropdownMenuTrigger,
} from '~/components/ui/dropdown-menu';
import { MoreHorizontal } from 'lucide-react';
import { MoreVertical } from 'lucide-react';
import { AddressDialog } from '~/components/common/modal/addressModal';
const countries = countryJSON.map((item) => item.country);

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
      <div className="grid gap-4 items-center md:items-start grid-cols-1 sm:grid-cols-2 xl:grid-cols-3">
        {addresses?.map((address) => (
          <CustomerAddress key={address.id} {...address} refetch={refetch} />
        ))}

        {addresses && addresses?.length < 4 ? (
          <div className="h-60 max-w-[256px] w-64  rounded-lg grid items-center justify-center border border-primary mx-auto sm:mx-0 ">
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
  is_default: boolean;
};

function CustomerAddress(props: CustomerAddressType) {
  const { toast } = useToast();
  const { lang } = useSelector((state: RootState) => state.layout);
  const [title, setTitle] = useState('');
  const [type, setType] = useState('');
  const [isModal, setIsModal] = useState(false);
  const [isEdit, setEdit] = useState(false);

  const handleCmsStatus = () => {
    setTitle('Default Address');
    setType(props?.address_type);
    setIsModal(!isModal);
  };

  const handleChange = () => {
    setEdit(!isEdit);
  };

  const currentAddress = addressType.find(
    (address) => address.value === props.address_type,
  );

  return (
    <form className="h-60 flex mx-auto sm:mx-0">
      <div
        className={`w-64 rounded-md border border-border-[1px] p-4  border-primary
        flex flex-col`}
      >
        <div className="flex justify-between items-center">
          <div className="flex  items-start gap-2">
            <p className="text-md  font-bold tracking-tight lead">
              {currentAddress?.label}{' '}
            </p>
            {props?.is_default ? (
              <div className="bg-red-500 h-3 w-3 rounded-full" />
            ) : null}
          </div>
          <DropdownMenu>
            <DropdownMenuTrigger asChild>
              <Button variant="ghost" className="h-8 w-8 p-0 bg-border">
                <span className="sr-only">Open menu</span>
                {/* <MoreVertical className="h-4 w-4 text-lg" /> */}
                <MoreHorizontal className="h-4 w-4 text-lg" />
              </Button>
            </DropdownMenuTrigger>
            <DropdownMenuContent align="end">
              <DropdownMenuItem
                onClick={handleChange}
                className="flex items-center gap-2 cursor-pointer"
                // onClick={() => handleView(row?.original, 'view')}
              >
                <span>
                  <i className={`fas fa-pencil`}></i>
                </span>
                <p className="text-xs">
                  {langContent[lang.lang].MyAccount.AddressView.EDIT_BUTTON}
                </p>
              </DropdownMenuItem>
              {!props?.is_default && (
                <>
                  <DropdownMenuSeparator />

                  <DropdownMenuItem
                    onClick={handleCmsStatus}
                    className="flex items-center gap-2 cursor-pointer"
                    // onClick={() => handleView(row?.original, 'view')}
                  >
                    Set Default
                  </DropdownMenuItem>
                </>
              )}
            </DropdownMenuContent>
          </DropdownMenu>
        </div>

        <div className="w-full h-full font-light text-[#eaeaea] flex flex-col pt-3 text-sm">
          <div className="space-y-1" dir="ltr">
            <p className="text-ellipsis whitespace-nowrap overflow-hidden">
              {' '}
              {langContent[lang.lang].MyAccount.AddressView.BOX as string}{' '}
              {props.postal_code}
            </p>
            <p className="text-ellipsis whitespace-nowrap overflow-hidden">
              {props?.phone_code}
              {props?.phone_number}
            </p>
            <p className="text-ellipsis whitespace-nowrap overflow-hidden">
              {props?.country}
            </p>
            <p className="text-ellipsis whitespace-nowrap overflow-hidden">
              {props?.state}
            </p>
            <p className="text-ellipsis whitespace-nowrap overflow-hidden">
              {props?.city}
            </p>
            <p className="text-ellipsis whitespace-nowrap overflow-hidden">
              {' '}
              {props?.street_address_1}
            </p>
            <p className="text-ellipsis whitespace-nowrap overflow-hidden">
              {props?.street_address_2}
            </p>
          </div>
        </div>
      </div>
      <AddressDialog
        isModal={isModal}
        setIsModal={setIsModal}
        title={title}
        type={type}
        openChangeHandler={handleCmsStatus}
        {...props}
      />
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
