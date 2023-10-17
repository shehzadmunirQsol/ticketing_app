import { useState } from 'react';
import CmsForm from '~/components/common/forms/cmsForm';
import GlobalBack from '~/components/common/globalBack';
import { LanguageInterface } from '~/components/common/language_select';

export default function EditCms() {
  const [language, setLanguage] = useState<LanguageInterface>({
    id: 1,
    code: 'en',
  });

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
