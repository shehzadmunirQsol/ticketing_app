import React from 'react';
import BannerTitle from '~/components/common/banner_title';
import CashBg from '~/public/assets/cash_bg.png';
const CashPage = () => {
  return (
  <div className='h-screen'>
    <BannerTitle image={CashBg} text={"Cash"}/>
  </div>
  );
};

export default CashPage;
