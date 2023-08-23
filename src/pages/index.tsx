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
    <div className="bg-background h-auto justify-center items-center     ">
      <Home />
    </div>
  );
};

export default IndexPage;
