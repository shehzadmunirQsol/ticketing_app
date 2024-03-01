import React from 'react';
import { useSelector } from 'react-redux';
import { RootState } from '~/store/store';
import Content from './content';

export default function Sidebar() {
  const { isSidebarOpen } = useSelector(
    (state: RootState) => state.adminLayout,
  );

  return (
    <div
      className={`${
        isSidebarOpen ? 'w-64' : 'w-20 overflow-hidden'
      } transition-all duration-150 p-4 space-y-2 text-grey bg-background h-full hidden xl:block overflow-y-scroll scroll-hide`}
    >
      <Content />
    </div>
  );
}
