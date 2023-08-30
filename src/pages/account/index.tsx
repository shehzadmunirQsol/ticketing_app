import React from 'react';
import { NextPageWithLayout } from '../_app';
import CashPage from '~/components/app/cash';
import Account from '~/components/app/account/index';

const IndexPage: NextPageWithLayout = () => {
  return (
    <>
      <Account />
    </>
  );
};

export default IndexPage;
