import React, { useEffect, useState } from 'react';
import { Button } from '@/ui/button';
import LogoImage from '~/public/assets/logo.png';
import Image from 'next/image';
import { useDispatch, useSelector } from 'react-redux';
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
import langContent from '~/locales';
import { addCart } from '~/store/reducers/cart';
import { userLogout } from '~/store/reducers/auth';
import { useToast } from '~/components/ui/use-toast';
import { trpc } from '~/utils/trpc';

export default function Header() {
  const router = useRouter();
  const { isLogin } = useSelector((state: RootState) => state.auth);
  const { lang } = useSelector((state: RootState) => state.layout);
  const { count } = useSelector((state: RootState) => state.cart);

  const [color, setColor] = useState(false);
  const [nav, setNav] = useState<boolean>(false);

  const dispatch = useDispatch();

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

  return (
    <div
      className={`fixed max-w-[1600px] w-full z-50 top-0 h-24  duration-500 
      ${
        router.route == '/'
          ? color
            ? 'bg-background-footer shadow-xl'
            : 'bg-transparent'
          : 'bg-background-footer'
      }  
       transform ease-in-out`}
    >
      <div
        className={`py-8 px-4 md:px-14 w-full flex items-center justify-between `}
      >
        <Link onClick={() => setNav(false)} href="/" className="z-50">
          <Image
            src={LogoImage}
            alt="Logo Image"
            width={150}
            height={140}
            className="h-4 sm:h-6 w-28 sm:w-56 "
          />
        </Link>
        <div className="hidden  mdx:flex gap-8 items-center justify-center">
          <ItemMenu />
          <div className="flex items-center justify-center gap-2">
            <Link href={'/cart'}>
              <Button
                variant="outline"
                size="icon_square"
                className="border-primary relative"
              >
                <i className="fa-solid fa-cart-shopping" />
                {count ? (
                  <span className="absolute -top-3 -right-3 inline-flex items-center my-auto justify-center bg-red-600 text-white rounded-full w-7 h-7 text-xs">
                    {count > 99 ? '99+' : count}
                  </span>
                ) : null}
              </Button>
            </Link>

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
              <SelectTrigger className="h-9 w-9 rounded-none border-primary text-center  justify-center text-gray-200">
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

        <div className="mdx:hidden flex justify-between gap-2 items-center">
          {nav ? (
            <Select value={lang.lang} onValueChange={toggleLanguageHandler}>
              <SelectTrigger className="z-50 h-9 w-9 rounded-none border-primary text-center  justify-center text-gray-200">
                <SelectValue placeholder="EN" />
              </SelectTrigger>
              <SelectContent>
                <SelectGroup>
                  <SelectItem value="en">EN</SelectItem>
                  <SelectItem value="ar">AR</SelectItem>
                </SelectGroup>
              </SelectContent>
            </Select>
          ) : (
            <Link className="z-[999]" href={'/cart'}>
              <Button
                variant="outline"
                size="icon_square"
                className="border-primary relative"
              >
                <i className="fa-solid fa-cart-shopping" />
                {count ? (
                  <span className="absolute -top-3 -right-3 inline-flex items-center my-auto justify-center bg-red-600 text-white rounded-full w-7 h-7 text-xs">
                    {count > 99 ? '99+' : count}
                  </span>
                ) : null}
              </Button>
            </Link>
          )}

          <div onClick={() => setNav((prev) => !prev)}>
            <Button
              variant="outline"
              size="icon_square"
              className="border-primary relative z-50"
            >
              {nav ? (
                <i className="fa-solid fa-xmark text-lg text-center text-gray-200"></i>
              ) : (
                <i className="fa-solid fa-bars text-lg text-center text-gray-200"></i>
              )}
            </Button>
          </div>
        </div>
      </div>

      <SideBarMenu nav={nav} closeFn={setNav} />
    </div>
  );
}

interface SideBarMenuProps {
  nav: boolean;
  closeFn: React.Dispatch<React.SetStateAction<boolean>>;
}

export const SideBarMenu: React.FC<SideBarMenuProps> = ({ nav, closeFn }) => {
  const { count } = useSelector((state: RootState) => state.cart);
  const { lang } = useSelector((state: RootState) => state.layout);
  const { isLogin } = useSelector((state: RootState) => state.auth);
  const [hide, setHide] = useState(false);
  const { toast } = useToast();
  const router = useRouter();
  const dispatch = useDispatch();

  const logout = trpc.customer.logout.useMutation({
    onSuccess: (res: any) => {
      console.log('return data', res);
    },
  });

  async function handleLogout() {
    try {
      const response = await logout.mutateAsync({});
      console.log('Response : ', response);
      toast({
        variant: 'success',
        title: 'Logout successfully! ',
      });
      localStorage.removeItem('winnar-token');
      localStorage.removeItem('customer');
      dispatch(userLogout());
      dispatch(
        addCart({
          id: null,
          customer_id: null,
          isDiscount: false,
          discount: 0,
          isPercentage: false,
          cartItems: [],
        }),
      );
      closeFn(false);
      router.replace('/login');
    } catch (error: any) {
      console.log('Error ', error);
      toast({
        variant: 'destructive',
        title: error.message,
      });
    }
  }

  useEffect(() => {
    if (window?.innerWidth !== undefined) {
      const changeColor = () =>
        window?.innerWidth >= 1000
          ? () => {
              setHide(true);
              closeFn(false);
            }
          : setHide(false);

      window.addEventListener('resize', changeColor);

      return () => {
        window.removeEventListener('resize', changeColor);
      };
    }
  }, []);

  useEffect(() => {
    window.addEventListener('resize', () => closeFn(false));
    return () => {
      window.removeEventListener('resize', () => closeFn(false));
    };
  }, []);

  const menuList = [
    {
      text: langContent[lang.lang].Header.title_one,
      link: '/cars',
    },
    {
      text: langContent[lang.lang].Header.title_two,
      link: '/cash',
    },
    {
      text: langContent[lang.lang].Header.title_three,
      link: '/winners',
    },
    {
      text: langContent[lang.lang].Header.title_four,
      link: '/about-us',
    },
    {
      text: 'FAQ',
      link: '/faq',
    },
    {
      text: langContent[lang.lang].Header.title_five,
      link: '/contact-us',
    },
    {
      text: isLogin
        ? langContent[lang.lang].Header.sub_title_seven
        : langContent[lang.lang].Header.title_seven,
      link: isLogin ? '/account' : '/login',
    },
  ];

  return (
    <ul
      className={`z-10 pb-4 block mdx:hidden  transition-all ease-linear font-sans  overflow-y-scroll scroll-hide ${
        nav
          ? 'absolute top-0 left-0 duration-100 w-full h-screen bg-background-footer flex flex-col justify-start items-center pt-24'
          : 'absolute top-24 left-[-100%]  duration-100'
      }`}
    >
      {menuList.map((item, i) => {
        return (
          <li
            key={i}
            className={`group py-3 text-lg w-full text-center ${
              menuList.length - 1 === i ? 'border-y-[1px]' : 'border-t-[1px]'
            }
             border-gray-700 hover:bg-primary hover:text-primary-foreground hover:border-transparent transition-colors duration-300 ease-in-out cursor-pointer`}
            onClick={() => {
              router.push(item.link);
              closeFn(false);
            }}
          >
            {item.text}
          </li>
        );
      })}
      {isLogin ? (
        <li
          className={`group py-3 text-lg w-full text-center border-b-[1px] 
           border-gray-700 hover:bg-primary hover:text-primary-foreground hover:border-transparent transition-colors duration-300 ease-in-out cursor-pointer`}
          onClick={handleLogout}
        >
          {' '}
          {lang.lang === 'ar' ? 'تسجيل خروج' : 'Logout'}
        </li>
      ) : null}{' '}
    </ul>
  );
};

export function ItemMenu() {
  const { lang } = useSelector((state: RootState) => state.layout);
  return (
    <div
      className="items-center justify-between hidden w-full md:flex md:w-auto "
      id="navbar-sticky"
    >
      <ul className="flex flex-col p-4 md:p-0  text-small font-normal  border  rounded-lg bg-transparent md:flex-row md:space-x-4 md:mt-0 md:border-0 md:bg-white dark:bg-transparent ">
        {langContent[lang.lang].Header.array.map((item) => {
          return (
            <li
              key={item.name}
              className="group border-b-2 border-transparent  "
            >
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
