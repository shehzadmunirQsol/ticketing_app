import { AlertDialog, AlertDialogContent, AlertDialogFooter } from '@/ui/alert-dialog';
import { ScrollArea } from '@radix-ui/react-scroll-area';
import { Button } from '~/components/ui/button';
import { ScrollBar } from '~/components/ui/scroll-area';

interface InvoiceModalInterface {
  open: boolean;
  text?: string;
  setIsModal: (data: boolean) => void;
}

export function InvoiceDialog(props: InvoiceModalInterface) {
  return (
    <AlertDialog open={props.open}>
      <AlertDialogContent className="max-w-[740px] justify-center py-4 px-6">
          {props.text && <div className="bg-white overflow-y-scroll w-[700px] h-[300px]" dangerouslySetInnerHTML={{ __html: props.text }} />}
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
