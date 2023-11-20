import { ScrollArea } from '@radix-ui/react-scroll-area';
import { useRouter } from 'next/router';
import React, { useEffect } from 'react';
import { useSelector } from 'react-redux';
import { RootState } from '~/store/store';
import { trpc } from '~/utils/trpc';
import { ScrollBar } from '../ui/scroll-area';
import { displayDate } from '~/utils/helper';
import LogoImage from '~/public/assets/logo.png';
import NextImage from '../ui/img';

const Invoice = () => {
  const { lang } = useSelector((state: RootState) => state.layout);
  const router = useRouter();
  const { id } = router.query;

  const { data: OrderApiData, isFetched } = trpc.order.getByID.useQuery(
    { order_id: Number(id) as any, lang_id: lang.lang_id },
    {
      refetchOnWindowFocus: false,

      enabled: id ? true : false,
    },
  );

  useEffect(() => {
    if (isFetched) {
      const printContents = document?.getElementById('divToPrint')
        ?.innerHTML as string;
      const originalContents = document.body.innerHTML;
      document.body.innerHTML = printContents;

      // print close action
      const handleAfterPrint = () => {
        window.close();
      };

      window.addEventListener('afterprint', handleAfterPrint);
      window.print();
      document.body.innerHTML = originalContents;
    }
  }, []);

  return (
    <>
      {OrderApiData && (
        <div
          className="bg-card h-full text-gray-400 rounded-lg mx-auto w-full px-8 py-10 sm:w-3/4 md:w-2/3"
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
            <div className=" xs:text-center md:text-left">
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

            <div className="w-full mb-8">
              <div className="flex justify-between font-bold uppercase py-2">
                <div className="flex-[2] text-start">Name</div>
                <div className="flex-1 text-center">Quantity</div>
                <div className="flex-1 text-center">Price</div>
                <div className="flex-1 text-right">Total</div>
              </div>
              <div className="mt-2">
                {OrderApiData?.data?.OrderEvent &&
                  OrderApiData?.data?.OrderEvent?.map(
                    (item: any, index: number) => (
                      <div key={index} className="flex gap-2 py-4">
                        <div className="flex-[2] text-start">
                          {item?.Event?.EventDescription[0]?.name}
                        </div>
                        <div className="flex-1 text-center">
                          {item?.quantity}
                        </div>
                        <div className="flex-1 text-center">
                          AED {item?.ticket_price.toFixed(2)}
                        </div>
                        <div className="flex-1 text-right">
                          AED {(item?.ticket_price * item?.quantity).toFixed(2)}
                        </div>
                      </div>
                    ),
                  )}
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
    </>
  );
};

export default Invoice;
