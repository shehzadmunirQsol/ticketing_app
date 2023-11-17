import { useRouter } from 'next/router';
import React, { useEffect, useState } from 'react';

import OrdersDataEventById from '~/components/common/table/ordersEventByIdTable';
import { trpc } from '~/utils/trpc';

function CustomerDetail() {
  const router = useRouter();
  const [filters, setFilters] = useState({
    customer_id: router?.query?.index ? (+router?.query?.index as any) : 0,
    status: 'current',
    first: 0,
    rows: 10,
    lang_id: 1,
  });

  const { data: customerDetail, isFetching } =
    trpc.customer.getCustomerDetail.useQuery(
      { ...filters },
      {
        refetchOnWindowFocus: false,
        enabled: filters.customer_id ? true : false,
      },
    );
  console.log({ customerDetail });

  useEffect(() => {
    if (router?.query?.index)
      setFilters({
        customer_id: (+router?.query?.index as any) ?? 0,
        status: 'current',
        first: 0,
        rows: 10,
        lang_id: 1,
      });
  }, [router?.query?.index]);
  return (
    <div className="justify-center items-center px-8 py-4">
      <div className="flex items-center justify-between mb-4">
        {/* <div className=" text-4xl font-semibold">Customer Details</div> */}
      </div>
      <div className=" grid grid-cols-12 gap-2">
        <div className=" col-span-12 md:col-span-4">
          <h1 className=" font-bold  text-2xl mb-4 "> Customer Details</h1>

          <CustomerDetailView customerDetail={customerDetail?.data} />
        </div>
        <div className="col-span-12 md:col-span-8">
          <h1 className=" font-bold px-2 text-2xl mb-4"> Order Events</h1>
          <OrdersDataEventById filters={filters} setFilters={setFilters} />
        </div>
      </div>
    </div>
  );
}
const CustomerDetailView = (props: any) => {
  console.log({ props });
  const profileFields = [
    { label: 'First Name', key: 'first_name' },
    { label: 'Last Name', key: 'last_name' },
    { label: 'Email', key: 'email' },
    { label: 'Phone No.', key: 'phone_number' },
  ];
  const addressFields = [
    { label: 'First Name', key: 'first_name' },
    { label: 'Last Name', key: 'last_name' },
    { label: 'Email', key: 'email' },
    { label: 'Phone No.', key: 'phone_number' },
  ];
  return (
    <div className="w-full px-4  bg-secondary/80 rounded-md text-sm ">
      <div className="grid grid-cols-1   gap-2">
        {profileFields.map((field) => (
          <div
            key={field.key}
            className="grid grid-cols-1 sm:grid-cols-2 md:grid-cols-3 my-2 items-center gap-2"
          >
            <div>{field.label}: </div>
            <div>{props?.customerDetail?.[field.key]}</div>
          </div>
        ))}
        <div className="grid grid-cols-2 md:grid-cols-3 my-2 items-center gap-2">
          <div>Default Address: </div>
          <div className=" col-span-2  lowercase">
            {`${
              props?.customerDetail?.CustomerAddress[0]?.street_address_1 ?? ''
            }, ${
              props?.customerDetail?.CustomerAddress[0]?.street_address_2 ?? ''
            }, ${props?.customerDetail?.CustomerAddress[0]?.city ?? ''}, ${
              props?.customerDetail?.CustomerAddress[0]?.state ?? ''
            }, ${props?.customerDetail?.CustomerAddress[0]?.country ?? ''}, ${
              props?.customerDetail?.CustomerAddress[0]?.postal_code ?? ''
            }`}
          </div>
        </div>
      </div>
    </div>
  );
};

export default CustomerDetail;
