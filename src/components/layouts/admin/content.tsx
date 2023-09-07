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

const SIDEBAR_DATA = [
  {
    key: '/admin/dashboard',
    icon: <i className="fa-solid fa-home p-4 rounded-full" />,
    title: 'Dashboard',
  },
  {
    key: '/admin/category',
    icon: <i className="fa-solid fa-images p-4 rounded-full"></i>,
    title: 'Category',
  },
  {
    key: '/admin/events',
    icon: <i className="fa-solid fa-calendar-days p-4 rounded-full"></i>,
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
    key: '/admin/orders',
    icon: <i className="fa-solid fa-chart-line p-4 rounded-full"></i>,
    title: 'Orders',
  },
  {
    key: '/admin/subscriptions',
    icon: <i className="fa-solid fa-money-check-dollar p-4 rounded-full"></i>,
    title: 'Subscriptions',
  },
  //   {
  //     key: '/client',
  //     icon: <i className="fa-solid fa-users p-4 rounded-full" />,
  //     title: 'Clients',
  //     child: [
  //       {
  //         key: '/admin/banners',

  //         icon: <i className="fa-solid fa-table p-4 rounded-full" />,
  //         title: 'Listing',
  //       },
  //       {
  //         key: '/admin/banners',

  //         icon: <i className="fa-solid fa-image p-4 rounded-full" />,
  //         title: 'Gallery',
  //       },
  //     ],
  //   },
  //   {
  //     key: '/notification',
  //     icon: <i className="fa-solid fa-bell p-4 rounded-full" />,
  //     title: 'Notification',
  //     child: [
  //       {
  //         key: '/admin/banners',

  //         icon: <i className="fa-solid fa-file p-4 rounded-full" />,
  //         title: 'Email',
  //       },
  //     ],
  //   },
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

export default function Content() {
  const [open, setOpen] = React.useState<string | null>('/dashboard');
  const [childActive, setChildActive] = React.useState<string>('/dashboard');

  const router = useRouter();

  function handleCollapse(key: string, item: any) {
    if (!item.child) {
      setChildActive(item.key);
    }
    if (open && key === open) {
      setOpen(null);
      return;
    }
    setOpen(key);
  }
  function handleClickChild(key: string) {
    setChildActive(key);
  }

  function routeHandler(item: { key: string; child: number | undefined }) {
    if (!item?.child) router.push(item.key);
  }
  return (
    <>
      {SIDEBAR_DATA.map((item, index) => {
        let active = childActive;
        if (!item.child) {
          active = active?.split('-')[0] as string;
        }
        return (
          <Collapsible.Root
            key={item.title}
            className="CollapsibleRoot"
            open={open === item.key}
            onOpenChange={() => handleCollapse(item.key, item)}
          >
            <Collapsible.Trigger asChild>
              <div
                className={`
                     ${
                       active === item.key
                         ? 'text-primary bg-secondary/80 font-semibold '
                         : ''
                     }
                        flex items-center mb-2 pr-4 rounded-full hover:bg-secondary/80 hover:text-primary align-middle justify-between cursor-pointer`}
                onClick={() =>
                  routeHandler({ key: item.key, child: item.child?.length })
                }
              >
                {/* <Drawer/> */}
                <div className="flex items-center gap-5">
                  {item.icon} {item.title}
                </div>
                {item.child && item.child.length > 0 && (
                  <button>
                    {open === item.key ? (
                      <i className="fa-solid fa-chevron-down" />
                    ) : (
                      <i className="fa-solid fa-chevron-up" />
                    )}
                  </button>
                )}
              </div>
            </Collapsible.Trigger>
            <Collapsible.Content className="space-y-2">
              {item.child &&
                item.child.length > 0 &&
                item.child.map((itemChild, index) => (
                  <Link
                    href={itemChild?.key}
                    onClick={() => handleClickChild(`${item.key}-${index}`)}
                    key={itemChild.title}
                    className={`
                        ${
                          active === `${item.key}-${index}`
                            ? 'text-primary bg-secondary/80 font-semibold '
                            : ''
                        }
                          flex items-center gap-5 cursor-pointer rounded-full hover:bg-secondary/80 hover:text-primary
                        `}
                  >
                    {itemChild.icon}
                    {itemChild.title}
                  </Link>
                ))}
            </Collapsible.Content>
          </Collapsible.Root>
        );
      })}
    </>
  );
}
