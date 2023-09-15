import React, { useEffect, useState } from 'react';
import { Button } from '@/ui/button';
import LogoImage from '~/public/assets/logo.png';
import Image from 'next/image';
import { useDispatch, useSelector } from 'react-redux';
import {
  DropdownMenu,
  DropdownMenuContent,
  DropdownMenuGroup,
  DropdownMenuItem,
  DropdownMenuPortal,
  DropdownMenuSeparator,
  DropdownMenuSub,
  DropdownMenuSubContent,
  DropdownMenuSubTrigger,
  DropdownMenuTrigger,
} from '@/ui/dropdown-menu';
import { User, ShoppingCart, Languages } from 'lucide-react';
import {
  Select,
  SelectContent,
  SelectGroup,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from '~/components/ui/select';
import { useRouter } from 'next/router';
import { toggleLang } from '~/store/reducers/layout';
import { RootState } from '~/store/store';
import Link from 'next/link';
import { useToast } from '~/components/ui/use-toast';
interface LinkItemProps {
  name: string;
  link: string;
  icon: string;
  disable?: boolean;
}

function Header() {
  const router = useRouter();
  const { isLogin } = useSelector((state: RootState) => state.auth);
  const { count } = useSelector((state: RootState) => state.cart);

  const [color, setColor] = useState(false);

  const dispatch = useDispatch();
  const { toast } = useToast();

  useEffect(() => {
    const changeColor = () =>
      window.scrollY >= 100 ? setColor(true) : setColor(false);

    window.addEventListener('scroll', changeColor);

    return () => {
      window.removeEventListener('scroll', changeColor);
    };
  }, []);

  function toggleLanguageHandler(lang: 'en' | 'ar') {
    const dir: 'ltr' | 'rtl' = lang === 'ar' ? 'rtl' : 'ltr';
    const lang_id: 1 | 2 = lang === 'en' ? 1 : 2;
    const language = { lang, dir, lang_id };

    dispatch(toggleLang(language));
  }

  function routeHandler(route: string) {
    if (!isLogin) {
      toast({
        variant: 'destructive',
        title: 'Login to your account first!',
      });
      return;
    }

    router.push(route);
  }

  return (
    <div
      className={`fixed w-full z-50 top-0 h-24  flex  items-center   ${
        router.route == '/'
          ? color
            ? '!bg-background-footer  duration-500 shadow-xl'
            : '!bg-transparent  duration-500'
          : '!bg-background-footer'
      }   transform ease-in-out justify-between py-8 px-4 md:px-14 `}
    >
      <Link href="/">
        <Image
          src={LogoImage}
          alt="Logo Image"
          width={150}
          height={140}
          className="h-4 sm:h-6 w-28 sm:w-56 "
        />
      </Link>
      <div className="hidden  mdx:flex gap-8 items-center justify-center">
        <ItemMenuDemo />
        <div className="flex items-center justify-center gap-2">
          <Button
            variant="outline"
            size="icon_square"
            className="border-primary relative"
            onClick={() => routeHandler('/cart')}
          >
            <i className="fa-solid fa-cart-shopping" />
            {count ? (
              <span className="absolute -top-3 -right-3 inline-flex items-center my-auto justify-center bg-red-600 text-white rounded-full w-7 h-7 text-xs">
                {count > 99 ? '99+' : count}
              </span>
            ) : null}
          </Button>

          <Link href={isLogin ? '/account' : '/login'}>
            <Button
              variant="outline"
              size="icon_square"
              className="border-primary"
            >
              <i className="fa-solid fa-user" />
            </Button>
          </Link>
          <Select onValueChange={toggleLanguageHandler}>
            <SelectTrigger className="h-10 w-10 rounded-none border-primary text-gray-200">
              <SelectValue placeholder="EN" />
            </SelectTrigger>
            <SelectContent>
              <SelectGroup>
                <SelectItem value="en">EN</SelectItem>
                <SelectItem value="ar">AR</SelectItem>
              </SelectGroup>
            </SelectContent>
          </Select>
        </div>
      </div>
      <div className="z-50 mdx:hidden">
        <DropdownMenuDemo />
      </div>
    </div>
  );
}

export default Header;

export function DropdownMenuDemo() {
  const [click, setClick] = useState(false);
  const dispatch = useDispatch();
  function toggleLanguageHandler(lang: 'en' | 'ar') {
    const dir: 'ltr' | 'rtl' = lang === 'ar' ? 'rtl' : 'ltr';
    const lang_id: 1 | 2 = lang === 'en' ? 1 : 2;
    const language = { lang, dir, lang_id };

    dispatch(toggleLang(language));
  }
  return (
    <DropdownMenu>
      <DropdownMenuTrigger asChild>
        <Button variant="outline">
          <i className="fas fa-bars"></i>
        </Button>
      </DropdownMenuTrigger>
      <DropdownMenuContent className="w-40" side={'bottom-end' as 'bottom'}>
        {/* <DropdownMenuLabel></DropdownMenuLabel> */}

        <DropdownMenuGroup>
          <DropdownMenuItem>
            <Link href="/cars">Cars</Link>
          </DropdownMenuItem>
          <DropdownMenuItem>
            <Link href="/cash">Cash</Link>
          </DropdownMenuItem>
          <DropdownMenuItem>
            <Link href="/winners">Winners</Link>
          </DropdownMenuItem>
          <DropdownMenuItem>
            <span>About Us</span>
          </DropdownMenuItem>
          <DropdownMenuItem>
            <span>FAQ</span>
          </DropdownMenuItem>
          <DropdownMenuItem>
            <ShoppingCart className="mr-2 h-4 w-4" />
            <Link href="/cart">Cart</Link>
          </DropdownMenuItem>
          <DropdownMenuItem>
            <User className="mr-2 h-4 w-4" />
            <Link href="/account">My Account</Link>
          </DropdownMenuItem>
        </DropdownMenuGroup>
        <DropdownMenuGroup>
          <DropdownMenuSub>
            <DropdownMenuSubTrigger>
              <Languages className="mr-2 h-4 w-4" />
              <span>Language</span>
            </DropdownMenuSubTrigger>
            <DropdownMenuPortal>
              <DropdownMenuSubContent className="!w-14">
                <DropdownMenuItem onClick={() => toggleLanguageHandler('en')}>
                  <span>EN</span>
                </DropdownMenuItem>
                <DropdownMenuItem onClick={() => toggleLanguageHandler('ar')}>
                  <span>AR</span>
                </DropdownMenuItem>
                <DropdownMenuSeparator />
              </DropdownMenuSubContent>
            </DropdownMenuPortal>
          </DropdownMenuSub>
        </DropdownMenuGroup>
      </DropdownMenuContent>
    </DropdownMenu>
  );
}

export function ItemMenuDemo() {
  const linkItems: Array<LinkItemProps> = [
    {
      name: 'Cars',
      link: '/cars',
      // link: `/`,
      icon: 'fas fa-house',
    },
    {
      name: 'Cash',
      link: `/cash`,
      // link: `/`,
      icon: 'fa-solid fa-globe',
    },
    {
      name: 'Winners',
      link: `/winners`,
      // link: `/`,
      icon: 'fa-sharp fa-regular fa-images',
    },
    {
      name: 'About Us',
      // link: `/about-us`,
      link: `/`,
      icon: 'fa-solid fa-image',
    },
    {
      name: 'FAQ',
      // link: `/faq`,
      link: `/`,
      icon: 'fa-solid fa-users',
    },
  ];
  return (
    <div
      className="items-center justify-between hidden w-full md:flex md:w-auto "
      id="navbar-sticky"
    >
      <ul className="flex flex-col p-4 md:p-0  text-small font-normal  border  rounded-lg bg-transparent md:flex-row md:space-x-4 md:mt-0 md:border-0 md:bg-white dark:bg-transparent ">
        {linkItems?.map((item, index) => {
          return (
            <li key={index} className="group border-b-2 border-transparent  ">
              <Link
                href={item?.link}
                className="flex flex-col py-2 pl-3 pr-4 text-gray-200  hover:underline hover:bg-gray-100  md:hover:bg-transparent  dark:text-white dark:hover:bg-gray-700 dark:hover:text-white md:dark:hover:bg-transparent dark:border-gray-700"
              >
                {item?.name}
                <span className="w-full h-1 bg-transparent group-hover:bg-teal mt-1 rounded-sm  "></span>
              </Link>
            </li>
          );
        })}
      </ul>
    </div>
  );
}
