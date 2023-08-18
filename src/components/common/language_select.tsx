import {
  Select,
  SelectItem,
  SelectTrigger,
  SelectContent,
  SelectGroup,
  SelectValue,
} from '@/ui/select';
import { useDispatch } from 'react-redux';
import { switchLanguage } from '~/store/reducers/admin_layout';
import { trpc } from '~/utils/trpc';

export default function LanguageSelect() {
  const { data } = trpc.language.get.useQuery();
  const dispatch = useDispatch();

  function onValueChange(value: string) {
    const language = data?.data.find((lang) => lang.code === value);

    dispatch(
      switchLanguage({
        id: language?.id as number,
        code: language?.code as 'en' | 'ar',
      }),
    );
  }

  return (
    <Select onValueChange={onValueChange}>
      <SelectTrigger className="h-10 w-20">
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
