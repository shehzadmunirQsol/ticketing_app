import { useDispatch } from 'react-redux';
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
import { removeFromCart } from '~/store/reducers/cart';
import { trpc } from '~/utils/trpc';
import { LoadingDialog } from './loadingModal';
import Image from 'next/image';
import { displayDate } from '~/utils/helper';
import { ScrollArea, ScrollBar } from '~/components/ui/scroll-area';
import LogoImage from '~/public/assets/logo.png';

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
  const { toast } = useToast();
  const dispatch = useDispatch();
  const {
    data: OrderApiData,
    isFetched,
    isLoading,
    isFetching,
  } = trpc.order.getByID.useQuery(
    { order_id: props?.selectedItem?.id },
    {
      refetchOnWindowFocus: false,

      enabled: props?.selectedItem?.id ? true : false,
    },
  );

  return (
    <>
      <Dialog open={props?.isModal} onOpenChange={(e) => props.setIsModal(e)}>
        <DialogContent className=" my-auto h-[calc(100%-100px)]  overflow-y-hidden mb-2 ">
          <DialogFooter className=" sm:justify-start items-start w-full   ">
            {/* <Button onClick={()=>onPrint()} type="submit">Print</Button> */}
          </DialogFooter>
          <DialogHeader className="">
            {/* <DialogTitle>Remove Item</DialogTitle> */}
          </DialogHeader>
          <DialogDescription className="relative bg-card h-full rounded-lg  overflow-y-scroll   scroll-hide">
            {OrderApiData && (
              <div
                className="bg-card h-full text-gray-400 rounded-lg shadow-lg px-8 py-10 max-w-xl mx-auto  "
                id="divToPrint"
              >
                <div className="flex flex-col md:flex-row items-center justify-between mb-8">
                  <div className="flex items-center">
                    <Image
                      className="h-16  object-contain mr-2"
                      src={LogoImage}
                      alt="Logo"
                    />
                    {/* <div className=" font-semibold text-lg">
                Your Company Name
              </div> */}
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
                  <div className=" mb-2">
                    {OrderApiData?.data?.first_name +
                      ' ' +
                      OrderApiData?.data?.last_name}
                  </div>
                  <div className=" mb-2">
                    {OrderApiData?.data?.street_address}
                  </div>
                  <div className=" mb-2">
                    {OrderApiData?.data?.city}, {OrderApiData?.data?.country}{' '}
                    {OrderApiData?.data?.postal_code}
                  </div>
                  <div className="">{OrderApiData?.data?.email}</div>
                </div>
                <ScrollArea className="w-full  ">
                  <ScrollBar orientation="horizontal"></ScrollBar>

                  <table className="w-full text-left mb-8  ">
                    <thead className="gap-2 space-x-2">
                      <tr>
                        <th className=" font-bold uppercase py-2">
                          Description
                        </th>
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
                  </table>
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
