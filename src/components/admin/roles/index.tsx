import React from 'react';
import { Button } from '../../ui/button';
import Link from 'next/link';
import RolesDataTable from '~/components/common/table/roles';

function Roles() {
  return (
    <div className="justify-center items-center px-8 py-4">
      <div className="flex items-center justify-between mb-4">
        <div className=" text-4xl font-semibold">Roles</div>
      </div>
      <RolesDataTable />
    </div>
  );
}

export default Roles;
