import { AlertDialog, AlertDialogContent } from '@/ui/alert-dialog';

interface LoadingDialogInterface {
  open: boolean;
  text?: string;
}

export function LoadingDialog(props: LoadingDialogInterface) {
  return (
    <AlertDialog open={props.open}>
      <AlertDialogContent>
        <div className="flex flex-col items-center justify-center">
          <div className="loader ease-linear rounded-full border-4 border-t-4 border-gray-200 h-12 w-12 mb-4" />
          <h2 className="text-gray-600 text-xl font-semibold">{props?.text}</h2>
        </div>
      </AlertDialogContent>
    </AlertDialog>
  );
}
