import { AlertDialog, AlertDialogContent, AlertDialogFooter } from '@/ui/alert-dialog';
import { Button } from '~/components/ui/button';

interface InvoiceModalInterface {
  open: boolean;
  text?: string;
  setIsModal: (data: boolean) => void;
}

export function InvoiceDialog(props: InvoiceModalInterface) {
  return (
    <AlertDialog open={props.open}>
      <AlertDialogContent>
        <div className="flex flex-col items-center justify-center">
          <h2 className="text-white text-xl font-semibold">{props?.text}</h2>
        </div>
        <AlertDialogFooter>
        <div className="flex items-center justify-end gap-4 mt-4">
          <Button
            variant="secondary"
            type="button"
            onClick={() => props.setIsModal(false)}>
                    Cancel
            </Button>
            <Button type="submit">
                  Print
             </Button>
            </div>
      </AlertDialogFooter>
      </AlertDialogContent>
    </AlertDialog>
  );
}
