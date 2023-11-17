import { useSelector } from 'react-redux';
import {
  Dialog,
  DialogContent,
  DialogDescription,
  DialogFooter,
  DialogHeader,
} from '~/components/ui/dialog';
import { trpc } from '~/utils/trpc';
import { LoadingDialog } from './loadingModal';
import { displayDate } from '~/utils/helper';
import { ScrollArea, ScrollBar } from '~/components/ui/scroll-area';
import LogoImage from '~/public/assets/logo.png';
import { RootState } from '~/store/store';
import { Button } from '~/components/ui/button';
import { useRouter } from 'next/router';
import Link from 'next/link';
import NextImage from '~/components/ui/img';
import { useState } from 'react';

interface OrderViewDialogInterface {
  selectedItem: any;
  isModal: boolean;
  title: string;
  setTitle: any;
  setSelectedItem: any;
  setIsModal: any;
  type: string;
  setType: any;
}

export function OrderViewDialog(props: OrderViewDialogInterface) {
  const router = useRouter();
  const { lang } = useSelector((state: RootState) => state.layout);
  const [selectedOrderEvent, setSelectedOrderEvent] = useState<any>({});

  const { data: OrderApiData, isFetching } = trpc.order.getByID.useQuery(
    { order_id: props?.selectedItem?.id, lang_id: lang.lang_id },
    {
      refetchOnWindowFocus: false,
      enabled: props?.selectedItem?.id ? true : false,
    },
  );

  const orderRoute = () => {
    if (router.asPath === '/admin/orders') {
      return `/admin/order-view/${props?.selectedItem?.id}`;
    } else {
      return `/order-view/${props?.selectedItem?.id}`;
    }
  };

  return (
    <>
      <Dialog open={props?.isModal} onOpenChange={(e) => props.setIsModal(e)}>
        <DialogContent className=" my-auto max-h-[800px] h-[calc(100%-100px)] max-w-xl md:max-w-[768px] overflow-y-hidden  ">
          <DialogFooter className=" sm:justify-start items-start w-full">
            <Link href={orderRoute()} target="_blank">
              <Button onClick={() => props.setIsModal(false)}>
                Print Invoice
              </Button>
            </Link>
          </DialogFooter>
          <DialogDescription className="relative bg-card h-full rounded-lg  overflow-y-scroll   scroll-hide">
            {OrderApiData && (
              <div
                className="bg-card h-full text-gray-400 rounded-lg  px-8 py-10  mx-auto  "
                id="divToPrint"
              >
                <div className="flex flex-col md:flex-row items-center justify-between mb-8">
                  <div className="flex items-center">
                    <NextImage
                      className="h-16  object-contain mr-2"
                      src={LogoImage}
                      alt="Logo"
                    />
                  </div>
                  <div className="text-gray-400 xs:text-center sm:text-left">
                    <div className="font-bold text-xl mb-2">INVOICE</div>
                    <div className="text-sm">
                      Date: {displayDate(OrderApiData?.data?.created_at)}
                    </div>
                    <div className="text-sm">
                      Invoice: #INV00{OrderApiData?.data?.id}
                    </div>
                  </div>
                </div>
                <div className="border-b-2 border-gray-300 pb-8 mb-8">
                  <h2 className="text-2xl  font-bold mb-4">Bill To:</h2>
                  <p className=" ">
                    {OrderApiData?.data?.first_name +
                      ' ' +
                      OrderApiData?.data?.last_name}
                  </p>
                  <p className=" ">{OrderApiData?.data?.street_address}</p>
                  <p className=" ">
                    {OrderApiData?.data?.city}, {OrderApiData?.data?.country}{' '}
                    {OrderApiData?.data?.postal_code}
                  </p>
                  <p className="mt-2">{OrderApiData?.data?.email}</p>
                </div>
                <ScrollArea className="w-full  ">
                  <ScrollBar orientation="horizontal"></ScrollBar>

                  <div className="w-full mb-8">
                    <div className="flex justify-between font-bold uppercase py-2">
                      <div className="flex-[2] text-start">Name</div>
                      <div className="flex-1 text-center">Quantity</div>
                      <div className="flex-1 text-center">Price</div>
                      <div className="flex-1 text-right">Total</div>
                    </div>

                    {isFetching ? null : (
                      <div className="mt-2">
                        {OrderApiData?.data?.OrderEvent &&
                          OrderApiData?.data?.OrderEvent?.map(
                            (item: any, index: number) => (
                              <div
                                key={index}
                                className="flex gap-2 py-2 sm:py-4"
                              >
                                <div className="flex-[2] text-start">
                                  {item?.Event?.EventDescription[0]?.name}
                                </div>
                                <div
                                  onClick={() => setSelectedOrderEvent(item)}
                                  className="flex-1 text-center cursor-pointer duration-150 hover:text-primary"
                                >
                                  {item?.quantity}
                                </div>
                                <div className="flex-1 text-center">
                                  AED {item?.ticket_price.toFixed(2)}
                                </div>
                                <div className="flex-1 text-right">
                                  AED{' '}
                                  {(
                                    item?.ticket_price * item?.quantity
                                  ).toFixed(2)}
                                </div>
                              </div>
                            ),
                          )}
                      </div>
                    )}
                  </div>
                </ScrollArea>

                <div className=" flex justify-between items-center">
                  <div></div>
                  <div>
                    <div className="flex justify-between items-center mb-6">
                      <div className=" mr-2">Subtotal:</div>
                      <div className="">
                        AED {(OrderApiData?.data?.sub_total_amount).toFixed(2)}
                      </div>
                    </div>

                    <div className="flex justify-between items-center  mb-6">
                      <div className=" mr-2">Discount:</div>
                      <div className="">
                        AED{' '}
                        {OrderApiData?.data?.discount_amount > 0
                          ? (OrderApiData?.data?.discount_amount).toFixed(2)
                          : '0.00'}
                      </div>
                    </div>

                    <div className="flex justify-between items-center border-t-2 border-gray-300 mb-6">
                      <div className=" mr-2">Total:</div>
                      <div className=" font-bold text-lg">
                        AED {(OrderApiData?.data?.total_amount).toFixed(2)}
                      </div>
                    </div>
                  </div>
                </div>
              </div>
            )}
          </DialogDescription>
        </DialogContent>
      </Dialog>
      <ViewTickets
        selectedOrderEvent={selectedOrderEvent}
        setSelectedOrderEvent={setSelectedOrderEvent}
      />
      <LoadingDialog open={isFetching} text={'Loading data...'} />
    </>
  );
}

type ViewTicketsType = {
  selectedOrderEvent: any;
  setSelectedOrderEvent: any;
};

export function ViewTickets(props: ViewTicketsType) {
  const router = useRouter();

  const { data: eventTickets, isFetching } = trpc.eventTicket.get.useQuery(
    {
      event_id: props?.selectedOrderEvent?.Event?.id,
      order_event_id: props?.selectedOrderEvent?.id,
    },
    {
      refetchOnWindowFocus: false,
      enabled: props?.selectedOrderEvent?.Event?.id ? true : false,
    },
  );

  function closeHandler() {
    props?.setSelectedOrderEvent({});
  }

  function printHandler() {
    const eventTicketPayload = {
      orderId: props?.selectedOrderEvent?.order_id,
      createdAt: props?.selectedOrderEvent?.created_at,
      eventName: props?.selectedOrderEvent?.Event?.EventDescription[0]?.name,
      tickets: eventTickets?.data?.map(
        (eventTicket) => eventTicket?.ticket_num,
      ),
    };

    localStorage.setItem('event_tickets', JSON.stringify(eventTicketPayload));
    props?.setSelectedOrderEvent({});
  }

  const orderRoute = () => {
    if (router.asPath?.includes('/admin/customers/detail')) {
      return `/admin/tickets-view`;
    } else {
      return `/tickets-view`;
    }
  };

  return (
    <>
      <Dialog
        open={!!props?.selectedOrderEvent?.id}
        onOpenChange={closeHandler}
      >
        <DialogContent className="flex flex-col max-h-[800px] h-[calc(100%-100px)] max-w-xl md:max-w-[768px] overflow-y-hidden">
          <DialogHeader>
            <Link href={orderRoute()} target="_blank">
              <Button onClick={printHandler}>Print Invoice</Button>
            </Link>
          </DialogHeader>

          <DialogDescription className="relative w-full h-full">
            <div
              className="bg-card h-full text-gray-400 rounded-lg p-4 "
              id="divToPrint"
            >
              <div className="flex flex-col md:flex-row items-center justify-between mb-8">
                <div className="flex items-center">
                  <NextImage
                    className="h-16  object-contain mr-2"
                    src={LogoImage}
                    alt="Logo"
                  />
                </div>
                <div className="text-gray-400 xs:text-center sm:text-left">
                  <div className="font-bold text-xl mb-2">INVOICE</div>
                  <div className="text-sm">
                    Date: {displayDate(props?.selectedOrderEvent?.created_at)}
                  </div>
                  <div className="text-sm">
                    Invoice: #INV00{props?.selectedOrderEvent?.order_id}
                  </div>
                </div>
              </div>
              <div className="space-y-6">
                <h3 className="text-2xl text-center font-bold">
                  Ticket Number List
                </h3>
                {eventTickets?.data?.length ? (
                  <div className="space-y-4">
                    <h3 className="text-xl">
                      {
                        props?.selectedOrderEvent?.Event?.EventDescription[0]
                          ?.name
                      }
                    </h3>
                    <div className="grid grid-cols-4 gap-2 md:grid-cols-6">
                      {eventTickets?.data?.map((eventTicket) => (
                        <p className={`w-20`} key={eventTicket?.ticket_num}>
                          #{eventTicket?.ticket_num}
                        </p>
                      ))}
                    </div>
                  </div>
                ) : (
                  <p className="text-lg text-center my-auto">
                    No Tickets Found!
                  </p>
                )}
              </div>
            </div>
          </DialogDescription>
        </DialogContent>
      </Dialog>
      <LoadingDialog open={isFetching} text={'Loading data...'} />
    </>
  );
}
