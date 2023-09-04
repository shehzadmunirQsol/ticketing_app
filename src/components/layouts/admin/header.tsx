import React from 'react';
import { Button } from '@/ui/button';
import LogoImage from '~/public/assets/logo.png';
import Image from 'next/image';
import { useDispatch } from 'react-redux';
import { trpc } from '~/utils/trpc';
import { toggleSidebar } from '~/store/reducers/admin_layout';
import { useRouter } from 'next/router';
import * as Collapsible from '@radix-ui/react-collapsible';
import {
  Sheet,
  SheetContent,
  SheetDescription,
  SheetHeader,
  SheetTitle,
  SheetTrigger,
} from '~/components/ui/sheet';
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
// import { router } from '~/server/trpc';
import { formatTrpcError } from '~/utils/helper';
import toast from 'react-hot-toast';
import Link from 'next/link';
import Content from './content';
function Header() {
  const dispatch = useDispatch();

  function toggleSidebarHandler() {
    dispatch(toggleSidebar());
  }

  return (
    <div className="sticky top-0 flex z-40 items-center bg-background border-b border-input justify-between py-2 px-4 shadow-sm">
      <div className="z-50">
        {/* Conditionally render DrawerFunction */}
        <div className="md:hidden">
          <DrawerFunction />
        </div>

        {/* Render the icon button */}
        <Button
          onClick={toggleSidebarHandler}
          variant="outline"
          size="icon"
          className="md:inline hidden"
        >
          <i className="fa-solid fa-bars" />
        </Button>
      </div>

      <Image src={LogoImage} alt="Logoimage" width={150} height={140} />
      <DropdownMenuDemo />
    </div>
  );
}

export default Header;

export function DropdownMenuDemo() {
  const router = useRouter();

  const logout = trpc.admin.logout.useMutation({
    onSuccess: (res: any) => {
      console.log('return data', res);
    },
    onError(error) {
      console.log(error.message, 'ERROR');
    },
  });

  async function handleLogout() {
    console.log('Working');

    try {
      const response = await logout.mutateAsync({});
      console.log('Response : ', response);
      toast.success('Logout successfully!');
      localStorage.removeItem('winnar-admin-token');
      router.replace('/admin/login');
    } catch (error: any) {
      console.log('Error ', error);
      console.log('Error ', error);
      const errorMessage = formatTrpcError(error?.shape?.message);
      console.log('Error : ', errorMessage);
      toast.error(errorMessage);
    }
  }

  return (
    <DropdownMenu>
      <DropdownMenuTrigger asChild>
        <Button variant="outline" size="icon">
          <i className="fa-solid fa-user" />
        </Button>
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
          <span onClick={handleLogout}>Log out</span>
          <DropdownMenuShortcut>⇧⌘Q</DropdownMenuShortcut>
        </DropdownMenuItem>
      </DropdownMenuContent>
    </DropdownMenu>
  );
}

function DrawerFunction() {
  return (
    <Sheet>
      <SheetTrigger className="flex items-center mb-2  p-3 border-2 rounded-full hover:bg-secondary/80 hover:text-primary align-middle justify-between cursor-pointer">
        <i className="fa-solid fa-bars" />
      </SheetTrigger>
      <SheetContent side={'left'}>
        <SheetHeader>
          <SheetDescription className="pt-10">
            <Content />
          </SheetDescription>
        </SheetHeader>
      </SheetContent>
    </Sheet>
  );
}
