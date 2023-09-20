import { NextPageWithLayout } from '~/pages/_app';
import ProductDetail from '~/components/app/productDescription/index';

const ProductDetails: NextPageWithLayout = () => {
  return (
    <div className="bg-background flex flex-col   w-screen   ">
      <ProductDetail />
    </div>
  );
};

export default ProductDetails;
