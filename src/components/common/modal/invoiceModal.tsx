import {
  AlertDialog,
  AlertDialogContent,
  AlertDialogFooter,
} from '@/ui/alert-dialog';
import { ScrollArea } from '@radix-ui/react-scroll-area';
import { Button } from '~/components/ui/button';
import { ScrollBar } from '~/components/ui/scroll-area';
import { LoadingDialog } from './loadingModal';
import { trpc } from '~/utils/trpc';

interface InvoiceModalInterface {
  open: boolean;
  text?: string;
  setIsModal: (data: boolean) => void;
  loader: boolean;
  setInvoiceId: (data: number) => void;
  invoiceId: number;
}

export function InvoiceDialog(props: InvoiceModalInterface) {
  const { data: projectInvoiceData, isLoading: fetchingInvoiceData } =
    trpc.project.getInvoiceTickets.useQuery(
      {
        id: props?.invoiceId,
      },
      {
        refetchOnWindowFocus: false,
      },
    );
  return (
    <>
      <AlertDialog open={props.open}>
        <AlertDialogContent className="max-w-[740px] justify-center py-4 px-6">
          {props.loader ? (
            // Loader is true, show loader
            <div className="flex items-center justify-center">
              <div className="loader ease-linear rounded-full border-4 border-t-4 border-gray-200 h-12 w-12 mb-4" />
            </div>
          ) : (
            // Loader is false, show HTML content
            <div
              className="bg-white overflow-y-scroll w-[700px] h-[300px]"
              dangerouslySetInnerHTML={{ __html: props.text || '' }}
            />
          )}
          <AlertDialogFooter>
            <div className="flex items-center justify-end gap-4 mt-4">
              <Button
                variant="secondary"
                type="button"
                onClick={() => props.setIsModal(false)}
              >
                Cancel
              </Button>
              <Button type="submit">Print</Button>
            </div>
          </AlertDialogFooter>
        </AlertDialogContent>
      </AlertDialog>
      <LoadingDialog open={props?.loader} text={'Loading data...'} />
    </>
  );
}
