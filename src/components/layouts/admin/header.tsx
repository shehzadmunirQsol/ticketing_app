import React from 'react';
import { Button } from '@/ui/button';
import LogoImage from '~/public/assets/logo.png';
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
  SheetOverlay,
  SheetTitle,
  SheetTrigger,
} from '~/components/ui/sheet';
import {
  DropdownMenu,
  DropdownMenuContent,
  DropdownMenuItem,
  DropdownMenuLabel,
  DropdownMenuSeparator,
  DropdownMenuShortcut,
  DropdownMenuTrigger,
} from '@/ui/dropdown-menu';
import { LogOut } from 'lucide-react';
// import { router } from '~/server/trpc';
import { formatTrpcError } from '~/utils/helper';
import toast from 'react-hot-toast';
import Content from './content';
import { LoadingDialog } from '~/components/common/modal/loadingModal';
import {
  userAdminAuth,
  userAdminIsLogin,
} from '~/store/reducers/adminAuthSlice';
import NextImage from '~/components/ui/img';
function Header() {
  const dispatch = useDispatch();

  function toggleSidebarHandler() {
    dispatch(toggleSidebar());
  }

  return (
    <div className="sticky top-0 flex z-40 items-center bg-background border-b border-input justify-between py-2 px-4 shadow-sm">
      <div className="z-50">
        {/* Conditionally render DrawerFunction */}
        <div className="xl:hidden">
          <DrawerFunction />
        </div>

        {/* Render the icon button */}
        <Button
          onClick={toggleSidebarHandler}
          variant="outline"
          size="icon"
          className="xl:inline hidden border-border"
        >
          <i className="fa-solid fa-bars" />
        </Button>
      </div>

      <NextImage
        src={LogoImage}
        alt="Logoimage"
        className=" h-16 w-28"
        width={150}
        height={80}
      />
      <DropdownMenuDemo />
    </div>
  );
}

export default Header;

export function DropdownMenuDemo() {
  const router = useRouter();
  const dispatch = useDispatch();

  const logout = trpc.admin.logout.useMutation({
    onSuccess: (res: any) => {
      console.log('return data', res);
    },
    onError(error) {
      console.log(error.message, 'ERROR');
    },
  });

  async function handleLogout() {
    try {
      await logout.mutateAsync({});
      toast.success('Logout successfully!');
      localStorage.removeItem('winnar-admin-token');
      dispatch(userAdminAuth(null));
      dispatch(userAdminIsLogin(false));
      router.replace('/admin/login');
    } catch (error: any) {
      const errorMessage = formatTrpcError(error?.shape?.message);
      toast.error(errorMessage);
    }
  }

  return (
    <>
      <DropdownMenu>
        <DropdownMenuTrigger asChild>
          <Button variant="outline" size="icon">
            <i className="fa-solid fa-user" />
          </Button>
        </DropdownMenuTrigger>
        <DropdownMenuContent className="w-56">
          <DropdownMenuLabel>My Account</DropdownMenuLabel>
          <DropdownMenuSeparator />
          <DropdownMenuItem className="cursor-pointer" onClick={handleLogout}>
            <LogOut className="mr-2 h-4 w-4" />
            <span>Log out</span>
            <DropdownMenuShortcut>⇧⌘Q</DropdownMenuShortcut>
          </DropdownMenuItem>
        </DropdownMenuContent>
      </DropdownMenu>
      <LoadingDialog open={logout.isLoading} text={'Loading...'} />
    </>
  );
}

function DrawerFunction() {
  return (
    <Sheet>
      <SheetOverlay />
      <SheetTrigger className="flex items-center mb-2  p-3 border border-border rounded-full hover:bg-secondary/80 hover:text-primary align-middle justify-between cursor-pointer">
        <i className="fa-solid fa-bars" />
      </SheetTrigger>
      <SheetContent className=" w-64 border-border " side={'left'}>
        <SheetHeader className="sticky top-0"></SheetHeader>
        <SheetDescription className="pt-10  h-full overflow-y-scroll scroll-hide  ">
          <Content />
        </SheetDescription>
      </SheetContent>
    </Sheet>
  );
}
