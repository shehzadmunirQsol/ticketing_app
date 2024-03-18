import React from 'react';
import { Button } from '../../ui/button';
import Link from 'next/link';
import RolesDataTable from '~/components/common/table/roles';
import PasswordChange from './changePassword';
import EmailChange from './changeEmail';

function Profile() {
  return (
    <div className="justify-center items-center px-8 py-4">
      <div className="flex items-center justify-between mb-4">
        <div className=" text-4xl font-semibold">Profile</div>
      </div>
      {/* <RolesDataTable /> */}
      <div className="flex flex-col gap-4">
        <EmailChange />
        <PasswordChange />
      </div>
    </div>
  );
}

export default Profile;
