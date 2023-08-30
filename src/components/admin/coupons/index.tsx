import React from 'react';
import { Button } from '../../ui/button';
import Link from 'next/link';
import CouponsDataTable from '~/components/common/table/coupons';

function Coupons() {
  return (
    <div className="justify-center items-center px-8 py-4">
      <div className="flex items-center justify-between mb-4">
        <div className=" text-4xl font-semibold">Coupons</div>
        <Link href="/admin/category/add">
          <Button type="submit" variant={'clip'} className="w-28">
            Add
          </Button>
        </Link>
      </div>
      <CouponsDataTable />
    </div>
  );
}

export default Coupons;
