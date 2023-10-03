import React from 'react';
import { Button } from '../../ui/button';
import Link from 'next/link';
import EventsDataTable from '~/components/common/table/events';

function EventsForm() {
  return (
    <div className=" px-8 py-4">
      <div className="flex items-center justify-between mb-4">
        <div className=" text-4xl font-semibold">Products</div>
        <Link href="/admin/events/add">
          <Button type="submit" variant={'clip'} className="w-28">
            Add
          </Button>
        </Link>
      </div>
      <EventsDataTable />
    </div>
  );
}

export default EventsForm;
