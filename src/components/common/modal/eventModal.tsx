import { useRouter } from 'next/router';
import { Button } from '~/components/ui/button';
import {
  Dialog,
  DialogContent,
  DialogDescription,
  DialogFooter,
  DialogHeader,
  DialogTitle,
} from '~/components/ui/dialog';
import { useToast } from '~/components/ui/use-toast';
import { trpc } from '~/utils/trpc';
import { EventDataType } from '../table/events';
import { Input } from '~/components/ui/input';
import {
  Table,
  TableBody,
  TableCaption,
  TableCell,
  TableHead,
  TableHeader,
  TableRow,
} from '~/components/ui/table';
import NextImage from '~/components/ui/img';
import { renderNFTImage } from '~/utils/helper';
import { useRef, useState } from 'react';
import { useDebounce } from '~/hooks/useDebounce';
import { LoadingDialog } from './loadingModal';

interface SelectCustomerInterface {
  isModal: boolean;
  openChangeHandler: () => void;
  event_id: number;
  customer_id: number;
  customer_name: string;
  event_name: string;
  customer_email: string;
}

export function SelectWinnerDialog(props: SelectCustomerInterface) {
  const { toast } = useToast();
  const router = useRouter();

  const selectWinner = trpc.winner.selectWinner.useMutation();

  async function selectItemHandler() {
    try {
      const payload = {
        event_id: props?.event_id,
        customer_id: props?.customer_id,
        customer_name: props?.customer_name,
        event_name: props?.event_name,
        customer_email: props?.customer_email,
        ticket_num: Math.floor(Math.random() * 99999),
      };

      await selectWinner.mutateAsync(payload);
      toast({
        variant: 'success',
        title: 'Winner Selected successfully!',
      });
      router.replace('/admin/winners');
    } catch (error: any) {
      props?.openChangeHandler();
      toast({
        variant: 'destructive',
        title: error?.message ?? 'Something went wrong!',
      });
    }
  }

  return (
    <Dialog open={props?.isModal} onOpenChange={props.openChangeHandler}>
      <DialogContent className="">
        <DialogHeader>
          <DialogTitle>Select Winner</DialogTitle>
          <DialogDescription>
            <p className="text-lg">
              Are you sure, you want to select{' '}
              <strong className="text-primary">{props?.customer_name}</strong>{' '}
              as a winner for{' '}
              <strong className="text-primary">{props?.event_name}</strong>{' '}
              event?
            </p>
          </DialogDescription>
        </DialogHeader>
        <DialogFooter>
          <Button
            variant={'secondary'}
            type="button"
            onClick={props.openChangeHandler}
            disabled={selectWinner.isLoading}
          >
            Cancel
          </Button>
          <Button
            type="submit"
            onClick={selectItemHandler}
            disabled={selectWinner.isLoading}
          >
            Select
          </Button>
        </DialogFooter>
      </DialogContent>
    </Dialog>
  );
}

type SearchWinnerDialog = {
  event: EventDataType;
  open: boolean;
  openChangeHandler: () => void;
};

export function SearchWinnerDialog(props: SearchWinnerDialog) {
  const [ticketNumber, setTicketNumber] = useState(0);

  const debouncedTicketNumber = useDebounce<number>(ticketNumber, 300);

  const { toast } = useToast();
  const router = useRouter();
  const inputRef = useRef<HTMLInputElement>(null);

  const { data, isFetching } = trpc.eventTicket.getEventTicketCustomer.useQuery(
    {
      event_id: props?.event?.id,
      ticket_num: debouncedTicketNumber,
    },
    {
      refetchOnWindowFocus: false,
      enabled: props?.open && debouncedTicketNumber > 0,
    },
  );

  const selectWinner = trpc.winner.selectWinner.useMutation();

  async function selectItemHandler() {
    if (!data?.data?.Customer) return;
    try {
      const payload = {
        event_id: props?.event?.id,
        customer_id: data?.data?.Customer?.id,
        customer_name: data?.data?.Customer?.first_name ?? '',
        event_name: props?.event?.name,
        ticket_num: data?.data?.ticket_num,
        customer_email: data?.data?.Customer?.email,
      };

      await selectWinner.mutateAsync(payload);
      toast({
        variant: 'success',
        title: 'Winner Selected successfully!',
      });
      closeHandler();
      router.replace('/admin/winners');
    } catch (error: any) {
      closeHandler();
      toast({
        variant: 'destructive',
        title: error?.message ?? 'Something went wrong!',
      });
    }
  }

  function closeHandler() {
    if (selectWinner?.isLoading) return;
    setTicketNumber(0);
    props?.openChangeHandler();
  }

  return (
    <>
      <Dialog open={props?.open} onOpenChange={closeHandler}>
        <DialogContent className="max-w-[700px]   overflow-x-scroll no-scrollbar">
          <DialogHeader>
            <DialogTitle>Select Winner</DialogTitle>
            <DialogDescription className="space-y-4 py-4">
              <div
                onClick={() => inputRef?.current && inputRef?.current?.focus()}
                className="px-4 py-2 gap-4 rounded-sm bg-background-footer border-border flex items-center cursor-pointer"
              >
                <i className="fa fa-search text-xl" />
                <input
                  ref={inputRef}
                  type="number"
                  value={ticketNumber > 0 ? ticketNumber : ''}
                  onChange={(e) => setTicketNumber(+e.target.value)}
                  placeholder="Enter ticket number... "
                  className="flex-1 bg-transparent border-none outline-none"
                />
                {ticketNumber > 0 ? (
                  <i
                    onClick={() => setTicketNumber(0)}
                    className="fa fa-xmark text-xl"
                  />
                ) : null}
              </div>

              {isFetching ? (
                <h3 className="text-lg py-4 text-center">Searching...</h3>
              ) : debouncedTicketNumber && data?.data?.Customer?.first_name ? (
                <Table>
                  <TableCaption>
                    By pressing the Select button, following customer will be
                    selected the Winner.
                  </TableCaption>
                  <TableHeader>
                    <TableRow>
                      <TableHead className="min-w-[150px]">Event</TableHead>
                      <TableHead className="min-w-[120px]">
                        Customer Name
                      </TableHead>
                      <TableHead className="min-w-[120px]">
                        Customer Email
                      </TableHead>
                      <TableHead className="min-w-[100px] text-right">
                        Ticket No.
                      </TableHead>
                    </TableRow>
                  </TableHeader>
                  <TableBody>
                    <TableRow>
                      <TableCell className="font-medium">
                        <div className="flex items-center gap-4 text-ellipsis whitespace-nowrap overflow-hidden">
                          <NextImage
                            className="object-cover bg-ac-2 h-10 w-16 rounded-lg"
                            src={renderNFTImage(props?.event)}
                            alt={props?.event?.name}
                            width={100}
                            height={100}
                          />

                          <p className="w-20 text-ellipsis whitespace-nowrap overflow-hidden">
                            {props?.event?.name}
                          </p>
                        </div>
                      </TableCell>
                      <TableCell>{data?.data?.Customer?.first_name}</TableCell>
                      <TableCell>{data?.data?.Customer?.email}</TableCell>
                      <TableCell className="text-right">
                        #{data?.data?.ticket_num}
                      </TableCell>
                    </TableRow>
                  </TableBody>
                </Table>
              ) : debouncedTicketNumber && !data?.data?.Customer?.first_name ? (
                <h3 className="text-lg py-4 text-center">
                  No ticket or customer found!
                </h3>
              ) : (
                <h3 className="text-lg py-4 text-center">
                  Please Enter Ticket Number to Select Winner!
                </h3>
              )}
            </DialogDescription>
          </DialogHeader>
          <DialogFooter>
            <Button
              variant={'secondary'}
              type="button"
              onClick={props.openChangeHandler}
              disabled={isFetching || selectWinner?.isLoading}
            >
              Cancel
            </Button>
            <Button
              disabled={
                !data?.data?.Customer || isFetching || selectWinner?.isLoading
              }
              type="submit"
              onClick={selectItemHandler}
            >
              Select
            </Button>
          </DialogFooter>
        </DialogContent>
      </Dialog>
      <LoadingDialog
        open={selectWinner?.isLoading}
        text={'Selecting Winner...'}
      />
    </>
  );
}
