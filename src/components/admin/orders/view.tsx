import { useRouter } from 'next/router';
import React, { useState } from 'react';
import { trpc } from '~/utils/trpc';
import LogoImage from '~/public/assets/logo.png';
import Image from 'next/image';
import { displayDate } from '~/utils/helper';
import { ScrollArea, ScrollBar } from '~/components/ui/scroll-area';
import { LoadingDialog } from '~/components/common/modal/loadingModal';
import { Separator } from '~/components/ui/separator';

export default function OrderView() {
  const router = useRouter();
  const { index } = router.query;
  const initialOrderFilters: any = {};
  if (index) initialOrderFilters.order_id = +index;
  const {
    data: OrderApiData,
    isFetched,
    isLoading,
  } = trpc.order.getByID.useQuery(initialOrderFilters, {
    refetchOnWindowFocus: false,

    enabled: index ? true : false,
  });
  console.log({ OrderApiData });

  return (
    <div className="relative p-8 space-y-8 ">
      {OrderApiData && (
        <div className="bg-white rounded-lg shadow-lg px-8 py-10 max-w-xl mx-auto">
          <div className="flex flex-col md:flex-row items-center justify-between mb-8">
            <div className="flex items-center">
              <Image
                className="h-16  object-contain mr-2"
                src={LogoImage}
                alt="Logo"
              />
              {/* <div className="text-gray-700 font-semibold text-lg">
                Your Company Name
              </div> */}
            </div>
            <div className="text-gray-700 xs:text-center sm:text-left">
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
            <h2 className="text-2xl text-gray-700 font-bold mb-4">Bill To:</h2>
            <div className="text-gray-700 mb-2">
              {OrderApiData?.data?.first_name +
                ' ' +
                OrderApiData?.data?.last_name}
            </div>
            <div className="text-gray-700 mb-2">
              {OrderApiData?.data?.street_address}
            </div>
            <div className="text-gray-700 mb-2">
              {OrderApiData?.data?.city}, {OrderApiData?.data?.country}{' '}
              {OrderApiData?.data?.postal_code}
            </div>
            <div className="text-gray-700">{OrderApiData?.data?.email}</div>
          </div>
          <ScrollArea className="w-full ">
            <ScrollBar orientation="horizontal"></ScrollBar>

            <table className="w-full text-left mb-8  overflow-x-scroll">
              <thead>
                <tr>
                  <th className="text-gray-700 font-bold uppercase py-2">
                    Description
                  </th>
                  <th className="text-gray-700 font-bold uppercase py-2">
                    Quantity
                  </th>
                  <th className="text-gray-700 font-bold uppercase py-2">
                    Price
                  </th>
                  <th className="text-gray-700 font-bold uppercase py-2">
                    Total
                  </th>
                </tr>
              </thead>
              <tbody>
                {OrderApiData?.data?.OrderEvent &&
                  OrderApiData?.data?.OrderEvent?.map((item, index) => {
                    return (
                      <tr key={index}>
                        <td className="py-4 text-gray-700">
                          {item?.Event?.EventDescription[0]?.name}
                        </td>
                        <td className="py-4 text-gray-700">{item?.quantity}</td>
                        <td className="py-4 text-gray-700">
                          AED {item?.ticket_price.toFixed(2)}
                        </td>
                        <td className="py-4 text-gray-700">
                          AED {(item?.ticket_price * item?.quantity).toFixed(2)}
                        </td>
                      </tr>
                    );
                  })}
              </tbody>
            </table>
          </ScrollArea>

          <div className=" flex justify-between items-center">

            <div>
              <div className="flex justify-between items-center mb-6">
                <div className="text-gray-700 mr-2">Subtotal:</div>
                <div className="text-gray-700">
                  AED {(OrderApiData?.data?.sub_total_amount).toFixed(2)}
                </div>
              </div>

              {OrderApiData?.data?.discount_amount > 0 && (
                <div className="flex justify-between items-center  mb-6">
                  <div className="text-gray-700 mr-2">Discount:</div>
                  <div className="text-gray-700">
                    AED {(OrderApiData?.data?.discount_amount).toFixed(2)}
                  </div>
                </div>
              )}

              
              <div className="flex justify-between items-center mb-6 border-t-2  border-gray-300">
                <div className="text-gray-700 mr-2">Total:</div>
                <div className="text-gray-700 font-bold text-xl">
                  AED {(OrderApiData?.data?.total_amount).toFixed(2)}
                </div>
              </div>
            </div>
          </div>

          <div className="border-t-2 border-gray-300 pt-8 mb-8">
            <div className="text-gray-700 mb-2">
              Payment is due within 30 days. Late payments are subject to fees.
            </div>
            <div className="text-gray-700 mb-2">
              Please make checks payable to Your Company Name and mail to:
            </div>
            <div className="text-gray-700">
              123 Main St., Anytown, USA 12345
            </div>
          </div> 
        </div>
      )}
      <LoadingDialog open={isLoading} text={'Loading data...'} />
    </div>
  );
}
