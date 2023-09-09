import Link from 'next/link';
import { useState } from 'react';
import CmsForm from '~/components/common/forms/cmsForm';
import LanguageSelect, {
  LanguageInterface,
} from '~/components/common/language_select';
import { Button } from '~/components/ui/button';

export default function AddCms() {
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
        <h2 className="text-4xl font-medium">Add Cms</h2>
        {/* <LanguageSelect languageHandler={languageHandler} /> */}

        <div className="flex items-center  gap-4">
          <LanguageSelect languageHandler={languageHandler} />
        </div>
      </div>
      <CmsForm language={language} />
    </div>
  );
}
