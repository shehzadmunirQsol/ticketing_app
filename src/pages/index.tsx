import { NextPageWithLayout } from '~/pages/_app';
import Home from '~/components/app/home';
import { trpc } from '~/utils/trpc';

const IndexPage: NextPageWithLayout = () => {
  return (
    <>
      <Home />
    </>
  );
};

export default IndexPage;
