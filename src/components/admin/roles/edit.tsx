import React from 'react';
import { Button } from '../../ui/button';
import Link from 'next/link';
import RolesPermisionDataTable from '~/components/common/table/rolespermisions';

function EditRolePermisions() {
  return (
    <div className="justify-center items-center px-8 py-4">
      <RolesPermisionDataTable />
    </div>
  );
}

export default EditRolePermisions;
