import {
  Select,
  SelectItem,
  SelectTrigger,
  SelectContent,
  SelectGroup,
  SelectValue,
} from '@/ui/select';
import { trpc } from '~/utils/trpc';

export interface LanguageInterface {
  id: number;
  code: 'en' | 'ar';
}

interface LanguageSelectInterface {
  languageHandler: (params: LanguageInterface) => void;
}

export default function LanguageSelect(props: LanguageSelectInterface) {
  const { data } = trpc.language.get.useQuery();

  function onValueChange(value: string) {
    const language = data?.data.find((lang) => lang.code === value);

    props.languageHandler({
      id: language?.id as number,
      code: language?.code as 'en' | 'ar',
    });
  }

  return (
    <Select onValueChange={onValueChange}>
      <SelectTrigger className="bg-background h-10 w-24">
        <SelectValue placeholder="EN" />
      </SelectTrigger>
      <SelectContent>
        <SelectGroup>
          {data?.data?.map((lang) => (
            <SelectItem key={lang.code} value={lang.code}>
              {lang?.code?.toUpperCase()}
            </SelectItem>
          ))}
        </SelectGroup>
      </SelectContent>
    </Select>
  );
}
