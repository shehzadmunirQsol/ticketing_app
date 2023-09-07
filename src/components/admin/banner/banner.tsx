import React from 'react';
import { Button } from '../../ui/button';
import Link from 'next/link';
import DataTableBanner from '~/components/common/table/banner';

function Banner() {
  return (
    <div className="justify-center items-center px-8 py-4">
      <div className="flex items-center justify-between mb-4">
        <div className=" text-4xl font-semibold">Banner</div>
        <Link href="/admin/settings/banners/add">
          <Button type="submit" variant={'clip'} className="w-28">
            Add
          </Button>
        </Link>
      </div>
      <DataTableBanner />
    </div>
  );
}

export default Banner;
