import React from 'react';
import { Button } from '@/ui/button';
import LogoImage from '~/public/assets/logo.png';
import Image from 'next/image';
import { useDispatch } from 'react-redux';
import { toggleSidebar } from '~/store/reducers/admin_layout';

function Header() {
  const dispatch = useDispatch();

  function toggleSidebarHandler() {
    dispatch(toggleSidebar());
  }

  return (
    <div className="sticky top-0 flex bg-primary items-center bg-slate-300 dark:bg-black border-b border-color-brand-primary/50 justify-between py-2 px-5 shadow-sm">
      <Button onClick={toggleSidebarHandler} variant="outline" size="icon">
        <i className="fa-solid fa-bars" />
      </Button>
      <Image src={LogoImage} alt="Logo Image" width={150} height={140} />
      <Button variant="outline" size="icon">
        <i className="fa-solid fa-user" />
      </Button>
    </div>
  );
}

export default Header;
