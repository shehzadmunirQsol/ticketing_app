import * as Collapsible from '@radix-ui/react-collapsible';
import Link from 'next/link';
import { useRouter } from 'next/router';
import React from 'react';
import { useSelector } from 'react-redux';
import {
  Tooltip,
  TooltipContent,
  TooltipProvider,
  TooltipTrigger,
} from '~/components/ui/tooltip';
import { RootState } from '~/store/store';

export default function Content() {
  const { isSidebarOpen } = useSelector(
    (state: RootState) => state.adminLayout,
  );

  const [open, setOpen] = React.useState<string | null>('/admin/dashboard');
  const [childActive, setChildActive] =
    React.useState<string>('/admin/dashboard');

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
      {SIDEBAR_DATA.map((item) => {
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

                {isSidebarOpen ? (
                  <div className="flex items-center gap-5 ">
                    {' '}
                    {item.icon} {item.title}
                  </div>
                ) : (
                  <TooltipProvider>
                    <Tooltip>
                      <TooltipTrigger asChild>
                        <div className="flex items-center gap-5 ">
                          {item.icon}
                        </div>
                      </TooltipTrigger>
                      <TooltipContent side="right">
                        <p className="text-base">{item.title}</p>
                      </TooltipContent>
                    </Tooltip>
                  </TooltipProvider>
                )}

                {isSidebarOpen && item.child && item.child.length > 0 && (
                  <button>
                    {open === item.key ? (
                      <i className="fa-solid fa-chevron-up" />
                    ) : (
                      <i className="fa-solid fa-chevron-down" />
                    )}
                  </button>
                )}
              </div>
            </Collapsible.Trigger>
            <Collapsible.Content className="space-y-2">
              {item.child &&
                item.child.length > 0 &&
                item.child.map((childItem, index) => (
                  <Link
                    href={childItem?.key}
                    onClick={() => handleClickChild(`${item.key}-${index}`)}
                    key={childItem.title}
                    className={`
                        ${
                          active === `${item.key}-${index}`
                            ? 'text-primary bg-secondary/80 font-semibold '
                            : ''
                        }
                          flex items-center gap-5 cursor-pointer rounded-full hover:bg-secondary/80 hover:text-primary
                        `}
                  >
                    {isSidebarOpen ? (
                      <div className="flex items-center gap-5 ">
                        {' '}
                        {childItem.icon} {childItem.title}
                      </div>
                    ) : (
                      <TooltipProvider>
                        <Tooltip>
                          <TooltipTrigger asChild>
                            <div className="flex items-center gap-5 ">
                              {childItem.icon}
                            </div>
                          </TooltipTrigger>
                          <TooltipContent side="right">
                            <p className="text-base">{childItem.title}</p>
                          </TooltipContent>
                        </Tooltip>
                      </TooltipProvider>
                    )}
                  </Link>
                ))}
            </Collapsible.Content>
          </Collapsible.Root>
        );
      })}
    </>
  );
}

const SIDEBAR_DATA = [
  {
    key: '/admin/dashboard',
    icon: <i className="fa-solid fa-home p-4 rounded-full " />,
    title: 'Dashboard',
  },

  {
    key: '/admin/customers',
    icon: (
      <i className="fa-solid fa-users  px-3.5 py-4 text-md rounded-full text-center" />
    ),
    title: 'Customers',
  },
  {
    key: '/admin/projects',
    icon: <i className="fa-solid fa-diagram-project p-4 rounded-full" />,
    title: 'Projects',
  },
  {
    key: '/admin/tickets',
    icon: <i className="fa-solid fa-ticket p-4 rounded-full" />,
    title: 'Tickets',
  },
  {
    key: '/reports',
    icon: <i className="fa-solid fa-chart-line p-4 rounded-full" />,
    title: 'Reports',
    child: [
      {
        key: '/admin/settings/roles',

        icon: <i className={` fa-solid fa-users p-4 rounded-full`} />,
        title: 'Customer',
      },
      {
        key: '/admin/settings/resources',

        icon: <i className=" fa-solid fa-diagram-project p-4 rounded-full" />,
        title: 'Projects',
      },
      {
        key: '/admin/settings/resources',

        icon: <i className=" fa-solid fa-ticket p-4 rounded-full" />,
        title: 'Tickets',
      },
    ],
  },
  {
    key: '/settings',
    icon: <i className="fa-solid fa-gear p-4 rounded-full" />,
    title: 'Settings',
    child: [
      {
        key: '/admin/settings/roles',

        icon: <i className={` fa-solid fa-users-gear  p-4 rounded-full`} />,
        title: 'Roles',
      },
      {
        key: '/admin/settings/resources',

        icon: <i className=" fa-solid fa-folder-tree p-4 rounded-full" />,
        title: 'Resources',
      },
    ],
  },
];
