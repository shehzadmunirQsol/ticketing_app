import CmsForm from '~/components/common/forms/cmsForm';
import GlobalBack from '~/components/common/globalBack';
import { LanguageInterface } from '~/components/common/language_select';

export default function AddCms() {
  const language: LanguageInterface = {
    id: 1,
    code: 'en',
  };

  return (
    <div className="p-8 space-y-8">
      <div className="flex items-center justify-between">
        <h2 className="text-4xl font-medium">Add CMS</h2>
        <GlobalBack />
      </div>
      <CmsForm language={language} />
    </div>
  );
}
