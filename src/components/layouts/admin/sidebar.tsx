import * as Collapsible from '@radix-ui/react-collapsible';
import Link from 'next/link';
import { useRouter } from 'next/router';
import React from 'react';
import { useSelector } from 'react-redux';
import { RootState } from '~/store/store';
import {
  Sheet,
  SheetContent,
  SheetDescription,
  SheetHeader,
  SheetTitle,
  SheetTrigger,
} from '~/components/ui/sheet';
import Content from './content';

const SIDEBAR_DATA = [
  {
    key: '/admin/dashboard',
    icon: <i className="fa-solid fa-home p-4 rounded-full" />,
    title: 'Dashboard',
  },
  {
    key: '/admin/category',
    icon: <i className="fa-solid fa-home p-4 rounded-full" />,
    title: 'Category',
  },
  {
    key: '/admin/events',
    icon: <i className="fa-solid fa-home p-4 rounded-full" />,
    title: 'Events',
  },
  {
    key: '/admin/customers',
    icon: <i className="fa-solid fa-users p-4 rounded-full" />,
    title: 'Customers',
  },
  {
    key: '/admin/coupons',
    icon: <i className="fa-solid fa-tag p-4 rounded-full" />,
    title: 'Coupon',
  },
  {
    key: '/client',
    icon: <i className="fa-solid fa-users p-4 rounded-full" />,
    title: 'Clients',
    child: [
      {
        key: '/admin/banners',

        icon: <i className="fa-solid fa-table p-4 rounded-full" />,
        title: 'Listing',
      },
      {
        key: '/admin/banners',

        icon: <i className="fa-solid fa-image p-4 rounded-full" />,
        title: 'Gallery',
      },
    ],
  },
  {
    key: '/notification',
    icon: <i className="fa-solid fa-bell p-4 rounded-full" />,
    title: 'Notification',
    child: [
      {
        key: '/admin/banners',

        icon: <i className="fa-solid fa-file p-4 rounded-full" />,
        title: 'Email',
      },
    ],
  },
  {
    key: '/settings',
    icon: <i className="fa-solid fa-gear p-4 rounded-full" />,
    title: 'Settings',
    child: [
      {
        key: '/admin/settings/banners',

        icon: <i className="fa-solid fa-file p-4 rounded-full" />,
        title: 'Banner',
      },
      {
        key: '/admin/settings/spotlight',

        icon: <i className="fa-solid fa-file p-4 rounded-full" />,
        title: 'Spot Light',
      },
    ],
  },
];

export default function Sidebar() {
  const { isSidebarOpen } = useSelector(
    (state: RootState) => state.adminLayout,
  );


  return (
    <div
      className={`${
        isSidebarOpen ? 'w-64' : 'w-20 overflow-hidden'
      } duration-150 p-4 space-y-2 text-grey bg-background min-h-screen hidden xl:block`}
    >
      <Content  />
    </div>
  );
}
