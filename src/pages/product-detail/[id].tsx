import { NextPageWithLayout } from '~/pages/_app';
import { Button } from '@/ui/button';
import { useTheme } from 'next-themes';
import ProductDetail from '~/components/app/productDescription/index';

const ProductDetails: NextPageWithLayout = () => {
  const { theme, setTheme } = useTheme();

  function toggleTheme() {
    const sysTheme: string = theme === 'dark' ? 'light' : 'dark';
    setTheme(sysTheme);
  }

  return (
    <div className="bg-background h-auto justify-center items-center  w-screen   ">
      <div className="">
        {/* <Button onClick={toggleTheme}>Magic appears</Button> */}
        <ProductDetail />
      </div>
    </div>
  );
};

export default ProductDetails;
