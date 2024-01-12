import { ScrollArea } from '@radix-ui/react-scroll-area';
import { useRouter } from 'next/router';
import React, { useEffect } from 'react';
import { useSelector } from 'react-redux';
import { RootState } from '~/store/store';
import { trpc } from '~/utils/trpc';
import { ScrollBar } from '../ui/scroll-area';
import { displayDate, reduceVATAmount, getVATAmount, numberToWords } from '~/utils/helper';
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
          className="bg-card whiteText h-full text-gray-400 rounded-lg mx-auto w-full px-8 py-10 sm:w-3/4 md:w-2/3"
          id="divToPrint"
        >
          <div className="flex flex-col md:flex-row items-start md:items-center justify-between mb-5">
            <div className="flex flex-col mb-5 md:mb-0">
              <NextImage
                className="h-16  object-contain mr-2"
                src={LogoImage}
                alt="Logo"
              />
              <div className="text-sm whiteText">
                Winnar Trading LLC
              </div>
              <div className="text-sm greyText">
                Office 1716 - F17, The Bay Gate Tower, Al Mustaqbal Street, <br/> Business Bay, Dubai, 74490
              </div>
            </div>
            <div className="text-left">
              <div className="font-bold text-xl mb-1 whiteText">TAX INVOICE</div>
              <div className="text-sm greyText">
                Date: {displayDate(OrderApiData?.data?.created_at)}
              </div>
              <div className="text-sm greyText">
                Invoice #: INV00{OrderApiData?.data?.id}
              </div>
              <div className="text-sm greyText">
                TRN: 104151951100003
              </div>
            </div>
          </div>
          <div className="border-b-2 border-gray-300 text-sm pb-5">
            <h2 className="text-2xl  font-bold mb-4 whiteText">Bill To:</h2>
            <p className="greyText">
              {OrderApiData?.data?.first_name +
                ' ' +
                OrderApiData?.data?.last_name}
            </p>
            <p className="greyText">{OrderApiData?.data?.street_address}</p>
            <p className="greyText">
              {OrderApiData?.data?.city}, {OrderApiData?.data?.country}{' '}
              {OrderApiData?.data?.postal_code}
            </p>
            <p className="greyText">{OrderApiData?.data?.email}</p>
          </div>
          <ScrollArea className="w-full">
            <ScrollBar orientation="horizontal"></ScrollBar>

            <div className="w-full text-xs my-3">
              <div className="flex justify-between font-bold uppercase pb-2 whiteText">
                <div className="flex-[2] text-start">Name</div>
                <div className="flex-1 text-center">Quantity</div>
                <div className="flex-1 text-center">Unit Price <span className="block text-xxs">(AED)</span></div>
                <div className="flex-1 text-center">Sub Total <span className="block text-xxs">(AED)</span></div>
                <div className="flex-1 text-center">VAT (5%) <span className="block text-xxs">(AED)</span></div>
                <div className="flex-1 text-right">Total Amount <span className="block text-xxs">(AED)</span></div>
              </div>
              <div className="mt-0">
                {OrderApiData?.data?.OrderEvent &&
                  OrderApiData?.data?.OrderEvent?.map(
                    (item: any, index: number) => (
                      <div key={index} className="flex gap-2 py-2 greyText">
                        <div className="flex-[2] text-start">
                          {item?.Event?.EventDescription[0]?.name}
                        </div>
                        <div className="flex-1 text-center">
                          {item?.quantity}
                        </div>
                        <div className="flex-1 text-center">
                          {reduceVATAmount(item?.ticket_price).toFixed(2)}
                        </div>
                        <div className="flex-1 text-center">
                          {reduceVATAmount(item?.ticket_price * item?.quantity).toFixed(2)}
                        </div>

                        <div className="flex-1 text-center">
                          {getVATAmount(item?.ticket_price * item?.quantity).toFixed(2)}
                        </div>

                        <div className="flex-1 text-right">
                          {(item?.ticket_price * item?.quantity).toFixed(2)}
                        </div>
                      </div>
                    ),
                  )}
              </div>
            </div>
          </ScrollArea>


          <div className="w-full border-t-2 border-gray-300 text-xs">
            <div className="w-full my-2">
              <div className="flex justify-between py-2">
                <div className="flex-[2] text-start font-bold uppercase whiteText">Total Amount</div>
                <div className="flex-1"></div>
                <div className="flex-1"></div>
                <div className="flex-1 text-center greyText">{reduceVATAmount(OrderApiData?.data?.sub_total_amount).toFixed(2)}</div>
                <div className="flex-1 text-center greyText">{getVATAmount(OrderApiData?.data?.sub_total_amount).toFixed(2)}</div>
                <div className="flex-1 text-right greyText">{(OrderApiData?.data?.total_amount).toFixed(2)}</div>
              </div>

              <div className="flex justify-between py-2">
                <div className="flex-[2] text-start font-bold uppercase whiteText">Discount Coupon</div>
                <div className="flex-1"></div>
                <div className="flex-1"></div>
                <div className="flex-1"></div>
                <div className="flex-1"></div>
                <div className="flex-1 text-right greyText">
                  {OrderApiData?.data?.discount_amount > 0
                    ? (OrderApiData?.data?.discount_amount).toFixed(2)
                    : '0.00'}
                </div>
              </div>
            </div>
          </div>

          <div className="w-full border-t-2 border-gray-300 text-xs">
            <div className="w-full mt-8">
              <div className="flex justify-between font-bold uppercase py-2 whiteText">
                <div className="flex-[2] text-start">AED <span className='greyText'>{numberToWords(OrderApiData?.data?.total_amount)}</span></div>
                <div className="flex-1"></div>
                <div className="flex-1"></div>
                <div className="flex-1 text-center"></div>
                <div className="flex-1 text-center">VAT Amount <span>(AED)</span></div>
                <div className="flex-1 text-right">Total Payable <span>(AED)</span></div>
              </div>

              <div className="flex justify-between py-2">
                <div className="flex-[2]"></div>
                <div className="flex-1"></div>
                <div className="flex-1"></div>
                <div className="flex-1 text-center"></div>
                <div className="flex-1 text-center greyText">{getVATAmount(OrderApiData?.data?.sub_total_amount).toFixed(2)}</div>
                <div className="flex-1 text-right greyText">{(OrderApiData?.data?.total_amount).toFixed(2)}</div>
              </div>
            </div>
          </div>

          {/* <div className=" flex justify-between items-center">
            <div></div>
            <div>
              <div className="flex justify-between items-center mb-6">
                <div className="font-bold mr-2">Subtotal:</div>
                <div className="">
                  AED {reduceVATAmount(OrderApiData?.data?.sub_total_amount).toFixed(2)}
                </div>
              </div>

              <div className="flex justify-between items-center  mb-6">
                <div className="font-bold mr-2">Discount:</div>
                <div className="greyText">
                  AED{' '}
                  {OrderApiData?.data?.discount_amount > 0
                    ? (OrderApiData?.data?.discount_amount).toFixed(2)
                    : '0.00'}
                </div>
              </div>

              <div className="flex justify-between items-center mb-2">
                <div className="font-bold mr-2">VAT:</div>
                <div className="">
                  AED {getVATAmount(OrderApiData?.data?.sub_total_amount).toFixed(2)}
                </div>
              </div>

              <div className="flex justify-between items-center border-t-2 border-gray-300 mb-6">
                <div className="font-bold mr-2">Total:</div>
                <div className=" font-bold text-lg">
                  AED {(OrderApiData?.data?.total_amount).toFixed(2)}
                </div>
              </div>
            </div>
          </div> */}
        </div>
      )}
    </>
  );
};

export default Invoice;
