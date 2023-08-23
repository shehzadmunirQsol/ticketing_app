import React from 'react';
import { Button } from '../../ui/button';
import Link from 'next/link';

function EventsForm() {
  return (
    <div className="justify-center items-center px-8 py-4">
      <div className="flex items-center justify-between mb-4">
        <div className=" text-4xl font-semibold">Events</div>
        <Link href="/admin/events/add">
          <Button type="submit" variant={'clip'} className="w-28">
            Add
          </Button>
        </Link>
      </div>
    </div>
  );
}

export default EventsForm;
