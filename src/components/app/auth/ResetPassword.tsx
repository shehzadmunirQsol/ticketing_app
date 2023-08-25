import { useRouter } from 'next/router';
import { useToast } from '~/components/ui/use-toast';

export default function ResetPassword() {
  const { toast } = useToast();
  const router = useRouter();
  return (
    <div>
        <p className='mt-38'>reset password verification link</p>
    </div>
  );
}
