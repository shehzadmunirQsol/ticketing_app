import React from 'react';
import { Button } from '@/ui/button';
import LogoImage from '~/public/assets/logo.png';
import Image from 'next/image';
import { useDispatch } from 'react-redux';
import { toggleSidebar } from '~/store/reducers/admin_layout';
import {
  DropdownMenu,
  DropdownMenuContent,
  DropdownMenuGroup,
  DropdownMenuItem,
  DropdownMenuLabel,
  DropdownMenuPortal,
  DropdownMenuSeparator,
  DropdownMenuShortcut,
  DropdownMenuSub,
  DropdownMenuSubContent,
  DropdownMenuSubTrigger,
  DropdownMenuTrigger,
} from '@/ui/dropdown-menu';
import {
  Cloud,
  CreditCard,
  Github,
  Keyboard,
  LifeBuoy,
  LogOut,
  Mail,
  MessageSquare,
  Plus,
  PlusCircle,
  Settings,
  User,
  UserPlus,
  Users,
} from 'lucide-react';
import {
  Select,
  SelectContent,
  SelectGroup,
  SelectItem,
  SelectLabel,
  SelectTrigger,
  SelectValue,
} from '~/components/ui/select';
interface LinkItemProps {
  name: string;
  link: string;
  icon: string;
  disable?: boolean;
}
function Header() {
  const dispatch = useDispatch();

  function toggleSidebarHandler() {
    dispatch(toggleSidebar());
  }

  return (
    <div className="sticky top-0 h-[120px]  flex bg-primary items-center bg-transparent  dark:bg-black border-b border-color-brand-primary/50 justify-between py-8 px-6 shadow-sm">
      <Image
        src={LogoImage}
        alt="Logo Image"
        width={150}
        height={140}
        className="h-6 w-56"
      />
      <div className="  flex gap-8 items-center justify-center">
        <ItemMenuDemo />
        <div className="flex items-center justify-center gap-2">
          <Button variant="outline" size="icon_square">
            <i className="fa-solid fa-cart-shopping" />
          </Button>
          <Button variant="outline" size="icon_square">
            <i className="fa-solid fa-user" />
          </Button>
          <Select>
            <SelectTrigger className="h-10 w-10 rounded-none border-teal text-gray-200">
              <SelectValue placeholder="LN" />
            </SelectTrigger>
            <SelectContent>
              <SelectGroup>
                <SelectItem value="EN">EN</SelectItem>
                <SelectItem value="AR">AR</SelectItem>
              </SelectGroup>
            </SelectContent>
          </Select>
        </div>
      </div>
    </div>
  );
}

export default Header;

export function DropdownMenuDemo() {
  return (
    <DropdownMenu>
      <DropdownMenuTrigger asChild>
        <Button variant="outline">Open</Button>
      </DropdownMenuTrigger>
      <DropdownMenuContent className="w-56">
        <DropdownMenuLabel>My Account</DropdownMenuLabel>
        <DropdownMenuSeparator />
        <DropdownMenuGroup>
          <DropdownMenuItem>
            <User className="mr-2 h-4 w-4" />
            <span>Profile</span>
            <DropdownMenuShortcut>⇧⌘P</DropdownMenuShortcut>
          </DropdownMenuItem>
          <DropdownMenuItem>
            <CreditCard className="mr-2 h-4 w-4" />
            <span>Billing</span>
            <DropdownMenuShortcut>⌘B</DropdownMenuShortcut>
          </DropdownMenuItem>
          <DropdownMenuItem>
            <Settings className="mr-2 h-4 w-4" />
            <span>Settings</span>
            <DropdownMenuShortcut>⌘S</DropdownMenuShortcut>
          </DropdownMenuItem>
          <DropdownMenuItem>
            <Keyboard className="mr-2 h-4 w-4" />
            <span>Keyboard shortcuts</span>
            <DropdownMenuShortcut>⌘K</DropdownMenuShortcut>
          </DropdownMenuItem>
        </DropdownMenuGroup>
        <DropdownMenuSeparator />
        <DropdownMenuGroup>
          <DropdownMenuItem>
            <Users className="mr-2 h-4 w-4" />
            <span>Team</span>
          </DropdownMenuItem>
          <DropdownMenuSub>
            <DropdownMenuSubTrigger>
              <UserPlus className="mr-2 h-4 w-4" />
              <span>Invite users</span>
            </DropdownMenuSubTrigger>
            <DropdownMenuPortal>
              <DropdownMenuSubContent>
                <DropdownMenuItem>
                  <Mail className="mr-2 h-4 w-4" />
                  <span>Email</span>
                </DropdownMenuItem>
                <DropdownMenuItem>
                  <MessageSquare className="mr-2 h-4 w-4" />
                  <span>Message</span>
                </DropdownMenuItem>
                <DropdownMenuSeparator />
                <DropdownMenuItem>
                  <PlusCircle className="mr-2 h-4 w-4" />
                  <span>More...</span>
                </DropdownMenuItem>
              </DropdownMenuSubContent>
            </DropdownMenuPortal>
          </DropdownMenuSub>
          <DropdownMenuItem>
            <Plus className="mr-2 h-4 w-4" />
            <span>New Team</span>
            <DropdownMenuShortcut>⌘+T</DropdownMenuShortcut>
          </DropdownMenuItem>
        </DropdownMenuGroup>
        <DropdownMenuSeparator />
        <DropdownMenuItem>
          <Github className="mr-2 h-4 w-4" />
          <span>GitHub</span>
        </DropdownMenuItem>
        <DropdownMenuItem>
          <LifeBuoy className="mr-2 h-4 w-4" />
          <span>Support</span>
        </DropdownMenuItem>
        <DropdownMenuItem disabled>
          <Cloud className="mr-2 h-4 w-4" />
          <span>API</span>
        </DropdownMenuItem>
        <DropdownMenuSeparator />
        <DropdownMenuItem>
          <LogOut className="mr-2 h-4 w-4" />
          <span>Log out</span>
          <DropdownMenuShortcut>⇧⌘Q</DropdownMenuShortcut>
        </DropdownMenuItem>
      </DropdownMenuContent>
    </DropdownMenu>
  );
}

export function ItemMenuDemo() {
  const linkItems: Array<LinkItemProps> = [
    { name: 'Cars', link: '/', icon: 'fas fa-house' },
    {
      name: 'Cash',
      link: `/#`,
      icon: 'fa-solid fa-globe',
    },
    {
      name: 'Winners',
      link: `/#`,
      icon: 'fa-sharp fa-regular fa-images',
    },
    {
      name: 'About Us',
      link: `/#`,
      icon: 'fa-solid fa-image',
    },
    {
      name: 'FAQ',
      link: `/#`,
      icon: 'fa-solid fa-users',
    },
  ];
  return (
    <div
      className="items-center justify-between hidden w-full md:flex md:w-auto "
      id="navbar-sticky"
    >
      <ul className="flex flex-col p-4 md:p-0  text-small font-normal  border  rounded-lg bg-transparent md:flex-row md:space-x-4 md:mt-0 md:border-0 md:bg-white dark:bg-transparent ">
        {linkItems &&
          linkItems?.map((item, index) => {
            return (
              <li key={index} className="group border-b-2 border-transparent  ">
                <a
                  href="#"
                  className="flex flex-col py-2 pl-3 pr-4 text-gray-200   hover:bg-gray-100  md:hover:bg-transparent  dark:text-white dark:hover:bg-gray-700 dark:hover:text-white md:dark:hover:bg-transparent dark:border-gray-700"
                >
                  {item?.name}
                  <span className="w-full h-1 bg-transparent group-hover:bg-teal mt-1 rounded-sm  "></span>
                </a>
              </li>
            );
          })}
      </ul>
    </div>
  );
}
