import { useState } from 'react';
import CouponForm from '~/components/common/forms/coupon';
import { LanguageInterface } from '~/components/common/language_select';

export default function AddCoupon() {
  const [language, setLanguage] = useState<LanguageInterface>({
    id: 1,
    code: 'en',
  });

  return (
    <div className="p-8 space-y-8">
      <div className="flex items-center justify-between">
        <h2 className="text-4xl font-medium">Add Coupon</h2>
        {/* <LanguageSelect languageHandler={languageHandler} /> */}
      </div>
      <CouponForm />
    </div>
  );
}
