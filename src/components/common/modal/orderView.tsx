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
import Image from 'next/image';
import { displayDate } from '~/utils/helper';
import { ScrollArea, ScrollBar } from '~/components/ui/scroll-area';
import LogoImage from '~/public/assets/logo.png';
import { RootState } from '~/store/store';
import { Button } from '~/components/ui/button';
import { useRouter } from 'next/router';
import Link from 'next/link';

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

  const { data: OrderApiData, isFetching } = trpc.order.getByID.useQuery(
    { order_id: props?.selectedItem?.id, lang_id: lang.lang_id },
    {
      refetchOnWindowFocus: false,
      enabled: props?.selectedItem?.id ? true : false,
    },
  );
  console.log(router.asPath, 'router.asPath');



  const orderRoute = () => {
    if(router.asPath === '/admin/orders'){
       return `/admin/order-view/${props?.selectedItem?.id}`
    }else{
      return `/order-view/${props?.selectedItem?.id}`
    }    
  }
  


  return (
    <>
      <Dialog open={props?.isModal} onOpenChange={(e) => props.setIsModal(e)}>
        <DialogContent className=" my-auto max-h-[800px] h-[calc(100%-100px)]  overflow-y-hidden  ">
          <DialogFooter className=" sm:justify-start items-start w-full   ">
            {/* <Link
              href={`${
                router.asPath === '/admin/orders'
                  ? `/admin/order-view/${props?.selectedItem?.id}`
                  : `/order-view/${props?.selectedItem?.id}`
              }`}
            > */}
            <Link href={orderRoute()} target='_blank'>
              <Button >Print Invoice</Button>
            </Link>
            {/* </Link> */}
          </DialogFooter>
          <DialogDescription className="relative bg-card h-full rounded-lg  overflow-y-scroll   scroll-hide">
            {OrderApiData && (
              <div
                className="bg-card h-full text-gray-400 rounded-lg  px-8 py-10 max-w-xl mx-auto  "
                id="divToPrint"
              >
                <div className="flex flex-col md:flex-row items-center justify-between mb-8">
                  <div className="flex items-center">
                    <Image
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
                      Invoice #: INV00{OrderApiData?.data?.id}
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

                  {/* <table className="w-full text-left mb-8  ">
                    <thead className="gap-2 space-x-2">
                      <tr>
                        <th className=" font-bold uppercase py-2">Name</th>
                        <th className=" font-bold uppercase py-2">Quantity</th>
                        <th className=" font-bold uppercase py-2">Price</th>
                        <th className=" font-bold uppercase py-2">Total</th>
                      </tr>
                    </thead>
                    <tbody>
                      {OrderApiData?.data?.OrderEvent &&
                        OrderApiData?.data?.OrderEvent?.map(
                          (item: any, index: number) => {
                            return (
                              <tr key={index} className="gap-2 space-x-2">
                                <td className="py-4 ">
                                  {item?.Event?.EventDescription[0]?.name}
                                </td>
                                <td className="py-4 ">{item?.quantity}</td>
                                <td className="py-4 ">
                                  AED {item?.ticket_price.toFixed(2)}
                                </td>
                                <td className="py-4 ">
                                  AED{' '}
                                  {(
                                    item?.ticket_price * item?.quantity
                                  ).toFixed(2)}
                                </td>
                              </tr>
                            );
                          },
                        )}
                    </tbody>
                  </table> */}

<div className="w-full mb-8">
  <div className="flex justify-between font-bold uppercase py-2">
    <div className="flex-1">Name</div>
    <div className="flex-1">Quantity</div>
    <div className="flex-1">Price</div>
    <div className="flex-1">Total</div>
  </div>
  <div className="mt-2">
    {OrderApiData?.data?.OrderEvent &&
      OrderApiData?.data?.OrderEvent?.map((item: any, index: number) => (
        <div key={index} className="flex gap-2 py-4">
          <div className="flex-1">
            {item?.Event?.EventDescription[0]?.name}
          </div>
          <div className="flex-1">{item?.quantity}</div>
          <div className="flex-1">
            AED {item?.ticket_price.toFixed(2)}
          </div>
          <div className="flex-1">
            AED {(item?.ticket_price * item?.quantity).toFixed(2)}
          </div>
        </div>
      ))}
  </div>
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
      <LoadingDialog open={isFetching} text={'Loading data...'} />
    </>
  );
}
