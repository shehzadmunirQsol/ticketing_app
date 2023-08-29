import React from 'react';
import { Button } from '../../ui/button';
import Link from 'next/link';
import CustomersDataTable from '~/components/common/table/customers';

function Customers() {
  return (
    <div className="justify-center items-center px-8 py-4">
      <div className="flex items-center justify-between mb-4">
        <div className=" text-4xl font-semibold">Customers</div>
        <></>
      </div>
      <CustomersDataTable />
    </div>
  );
}

export default Customers;
