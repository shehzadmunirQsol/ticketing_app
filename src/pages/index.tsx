import { NextPageWithLayout } from '~/pages/_app';
import { Button } from '@/ui/button';
import { useTheme } from 'next-themes';
import Home from '~/components/app/home';

const IndexPage: NextPageWithLayout = () => {
  const { theme, setTheme } = useTheme();

  function toggleTheme() {
    const sysTheme: string = theme === 'dark' ? 'light' : 'dark';
    setTheme(sysTheme);
  }

  return (
    <div className="bg-slate-300 dark:bg-background    w-full  ">
      <div className="">
        {/* <Button onClick={toggleTheme}>Magic appears</Button> */}
        <Home />
      </div>
    </div>
  );
};

export default IndexPage;
