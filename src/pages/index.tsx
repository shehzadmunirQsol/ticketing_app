import { NextPageWithLayout } from '~/pages/_app';
import Home from '~/components/app/home';

const IndexPage: NextPageWithLayout = () => {
  return (
    <div className="bg-background h-auto justify-center items-center     ">
      <Home />
    </div>
  );
};

export default IndexPage;
