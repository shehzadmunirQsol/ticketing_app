import { useState } from 'react';
import CategoryForm from '~/components/common/forms/categoryForm';
import CouponForm from '~/components/common/forms/coupon';
import LanguageSelect, {
  LanguageInterface,
} from '~/components/common/language_select';

export default function AddCoupon() {
  const [language, setLanguage] = useState<LanguageInterface>({
    id: 1,
    code: 'en',
  });

  function languageHandler(params: LanguageInterface) {
    setLanguage(params);
  }

  return (
    <div className="p-8 space-y-8">
      <div className="flex items-center justify-between">
        <h2 className="text-4xl font-medium">Add Coupon</h2>
        {/* <LanguageSelect languageHandler={languageHandler} /> */}
      </div>
      <CouponForm language={language} />
    </div>
  );
}
