import { useState } from 'react';
import CategoryForm from '~/components/common/forms/categoryForm';
import LanguageSelect, {
  LanguageInterface,
} from '~/components/common/language_select';

export default function AddCategory() {
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
        <h2 className="text-4xl font-medium">Add Category</h2>
        <LanguageSelect languageHandler={languageHandler} />
      </div>
      <CategoryForm language={language} />
    </div>
  );
}