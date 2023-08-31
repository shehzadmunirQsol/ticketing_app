import React, { useState } from 'react';
import { useForm } from 'react-hook-form';
import { trpc } from '~/utils/trpc';

const AddressesView = () => {
  const [isEdit, setEdit] = useState(false);
  const [action, setAction] = useState({
    adding: 'ADD',
    save: 'SAVE',
  });

  const customerLoginDataJSON = localStorage.getItem('customer');
  const customerLocal = JSON.parse(customerLoginDataJSON as string);

  const { data } = trpc.customer.getAddress.useQuery(
    { customer_id: customerLocal.id },
    {
      refetchOnWindowFocus: false,
      onSuccess: (data) => {
        if (data !== null) {
          setAction({ ...action, adding: 'EDIT' });
        }
      },
    },
  );

  console.log({ data }, 'customer_data_res');
  return (
    <div className="py-4 px-6 text-[#eaeaea]">
      <p className="text-[#808080] text-sm">
        The following addresses will be used on the checkout page by default.
      </p>

      <div className="mt-4 flex ">
        <div
          className={`w-64   rounded-md border-[1px] p-4 ${
            data ? 'border-primary' : 'border-[#808080]'
          }  flex flex-col`}
        >
          <div className="flex justify-between items-center">
            <p className="text-md font-bold tracking-tight lead">
              Billing Address
            </p>
            <div
              onClick={() => setEdit(!isEdit)}
              className="px-2 py-1.5 bg-primary text-sm text-[#101417] font-extrabold tracking-tight leading-tight w-fit rounded-full flex justify-center gap-1 hover:cursor-pointer hover:opacity-90"
            >
              <span>
                <i className={`fas ${isEdit ?  'fa-pencil':'fa-plus' } `}></i>
              </span>
              <p className="text-xs">{isEdit ? action.save : action.adding}</p>
            </div>
          </div>

          <div className="w-full h-full font-light text-[#eaeaea] flex flex-col pt-3 text-sm">
            {data == undefined && (
              <p className=" h-24">
                You have not set up this type of address yet.
              </p>
            )}
            {data && !isEdit ? (
              <div className="h-fit ">
                <p>{data?.Customer.first_name} {data?.Customer.last_name}</p>
                <p>P.O Box {data.postal_code}</p>
                <p>{data?.street_address_1}</p>
                <p>{data?.phone_number}</p>
                <p>
                  {data?.city} {data?.country}
                </p>
              </div>
            ) : data || isEdit ? (
              <form className="h-fit ">
                <input
                  className="bg-primary-foreground p-0.5 font-sans"
                  value={data?.name}
                  placeholder="Name"
                  type="text"
                />
                <input
                  className="bg-primary-foreground p-0.5 font-sans"
                  value={`P.O Box ${data?.pobox}`}
                  placeholder="P.O Box"
                  type="text"
                />
                <input
                  className="bg-primary-foreground p-0.5 font-sans"
                  value={data?.street}
                  placeholder="Street Address"
                  type="text"
                />
                <input
                  className="bg-primary-foreground p-0.5 font-sans"
                  value={data?.number}
                  placeholder="Mobile"
                  type="tel"
                />
                <input
                  className="bg-primary-foreground p-0.5 font-sans"
                  value={data?.city}
                  placeholder="City"
                  type="text"
                />
                <input
                  className="bg-primary-foreground p-0.5 font-sans"
                  value={data?.country}
                  placeholder="Country"
                  type="text"
                />
              </form>
            ) : (
              ''
            )}
          </div>
        </div>
      </div>
    </div>
  );
};

export default AddressesView;
