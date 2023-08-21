import React from 'react';
import { Button } from '../../ui/button';
import Link from 'next/link';
import DataTableSpotLight from '~/components/common/spotlight_table';

function SpotLight() {
  return (
    <div className="justify-center items-center px-8 py-4">
      <div className="flex items-center justify-between mb-4">
        <div className=" text-4xl font-semibold">Spot Light</div>
        <Link href="/admin/settings/spotlight/add">
          <Button type="submit" variant={'clip'} className="w-28">
            Add
          </Button>
        </Link>
      </div>
      <DataTableSpotLight />
    </div>
  );
}

export default SpotLight;
