import * as Collapsible from '@radix-ui/react-collapsible';
import Link from 'next/link';
import { useRouter } from 'next/router';
import React from 'react';
import { useSelector } from 'react-redux';
import { RootState } from '~/store/store';

const SIDEBAR_DATA = [
  {
    key: '/admin/dashboard',
    icon: <i className="fa-solid fa-home p-4 rounded-full" />,
    title: 'Dashboard',
  },
  {
    key: '/admin/events',
    icon: <i className="fa-solid fa-home p-4 rounded-full" />,
    title: 'Events',
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
    ],
  },
];

export default function Sidebar() {
  const { isSidebarOpen } = useSelector(
    (state: RootState) => state.adminLayout,
  );

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
    <div
      className={`${
        isSidebarOpen ? 'w-64' : 'w-20 overflow-hidden'
      } duration-150 p-4 space-y-2 text-grey bg-background min-h-screen`}
    >
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
    </div>
  );
}
