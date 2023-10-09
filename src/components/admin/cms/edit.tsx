import Link from 'next/link';
import { useState } from 'react';
import CmsForm from '~/components/common/forms/cmsForm';
import GlobalBack from '~/components/common/globalBack';
import LanguageSelect, {
  LanguageInterface,
} from '~/components/common/language_select';
import { Button } from '~/components/ui/button';

export default function EditCms() {
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
        <h2 className="text-4xl font-medium">Edit CMS</h2>
        <GlobalBack />
      </div>
      <CmsForm language={language} />
    </div>
  );
}
