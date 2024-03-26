import {
  AlertDialog,
  AlertDialogContent,
  AlertDialogFooter,
} from '@/ui/alert-dialog';
import { Button } from '~/components/ui/button';
import { LoadingDialog } from './loadingModal';
import { trpc } from '~/utils/trpc';
import { useEffect, useState } from 'react';

interface InvoiceModalInterface {
  open: boolean;
  setIsModal: (data: boolean) => void;
  loader: boolean;
  setLoader: (data: boolean) => void;
  invoiceId: number;
}

export function InvoiceDialog(props: InvoiceModalInterface) {
  const [invoiceData, setInvoiceData] = useState<string>('');
  const { data: projectInvoiceData, isLoading: fetchingInvoiceData } =
    trpc.project.getInvoiceTickets.useQuery(
      {
        id: props?.invoiceId,
      },
      {
        refetchOnWindowFocus: false,
      },
    );

  function printHtmlContent(htmlContent: string) {
    const printWindow = window.open('', '_blank');
    printWindow?.document.write(htmlContent);
    // printWindow?.document.close();
    printWindow?.print();
  }
  useEffect(() => {
    if (projectInvoiceData?.data) {
      setInvoiceData(projectInvoiceData.data);
      props.setLoader(false);
    }
  }, [projectInvoiceData]);

  useEffect(() => {
    props.setLoader(true);
  }, [props.invoiceId]);

  return (
    <>
      <AlertDialog open={props.open}>
        <AlertDialogContent className="max-w-[740px] justify-center py-4 px-6">
          <div
            className="bg-white overflow-y-scroll w-[700px] h-[300px]"
            dangerouslySetInnerHTML={{
              __html: invoiceData,
            }}
          />
          <AlertDialogFooter>
            <div className="flex items-center justify-end gap-4 mt-4">
              <Button
                variant="secondary"
                type="button"
                onClick={() => props.setIsModal(false)}
              >
                Cancel
              </Button>
              <Button
                type="submit"
                onClick={() => {
                  printHtmlContent(invoiceData);
                }}
              >
                Print
              </Button>
            </div>
          </AlertDialogFooter>
        </AlertDialogContent>
      </AlertDialog>
      <LoadingDialog
        open={props?.loader && fetchingInvoiceData}
        text={'Loading data...'}
      />
    </>
  );
}
