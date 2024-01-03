import { ScrollArea } from '@radix-ui/react-scroll-area';
import Image from 'next/image';
import { useRouter } from 'next/router';
import { useDispatch, useSelector } from 'react-redux';
import { RootState } from '~/store/store';
import { trpc } from '~/utils/trpc';
import { ScrollBar } from '../ui/scroll-area';
import { displayDate } from '~/utils/helper';
import LogoImage from '~/public/assets/logo.png';
import Confetti from 'react-confetti';
import { useEffect, useState, useRef } from 'react';
import { setOrderID } from '~/store/reducers/cart';
import ProductSection from '../app/home/product_section';
import Glow from '~/components/common/glow';
import { Button } from '../ui/button';
import NextImage from '../ui/img';
import langContent from '~/locales';
import { URIGenerator, customTruncate, renderNFTImage } from '~/utils/helper';
// import carSound from '~/public/assets/car-sound.mp3';

export default function SuccessInvoice() {


  var fullUrl = "";
  if(typeof window !== 'undefined'){
    // fullUrl = window.location.protocol + "//" + window.location.host;
    fullUrl = window.location.host;
  }


  const { lang } = useSelector((state: RootState) => state.layout);
  const { orderID } = useSelector((state: RootState) => state.cart);

  const [recycle, setRecycle] = useState(true);
  const router = useRouter();
  const dispatch = useDispatch();

//TEST-------DELETE 
  // const { data: OrderApiData, isLoading } = trpc.order.getByID.useQuery(
  //   { order_id: 463, lang_id: 1 },
  //   {
  //     refetchOnWindowFocus: false,
  //     enabled: 463 > 0,
  //   },
  // );
//TEST-------DELETE



  const { data: OrderApiData, isLoading } = trpc.order.getByID.useQuery(
    { order_id: orderID, lang_id: lang.lang_id },
    {
      refetchOnWindowFocus: false,
      enabled: orderID > 0,
    },
  );

  useEffect(() => {

    const timeout = setTimeout(() => {
      setRecycle(false);
    }, 3000);

    return () => {
      clearTimeout(timeout);
      dispatch(setOrderID(0));
    };
  }, []);

  function routeHandler() {
    router.replace('/');
  }

  // const handleButtonClick = () => {
  //   const audio = new Audio(carSound);
  //   audio.play().catch(error => {
  //     console.error("Error playing audio:", error);
  //   });
  // };


  function padTicketNum(ticketNum: string | number) {
    const numDigits = ticketNum.toString().length;
    const zerosToAdd = Math.max(6 - numDigits, 0);
    return '0'.repeat(zerosToAdd) + ticketNum;
  }

  useEffect(() => {
 
    console.log(OrderApiData, "OrderApiData")

    if ('sendinblue' in window && window?.sendinblue) {
      const options: any = {weekday: 'long', day: 'numeric',month: 'long',year: 'numeric', hour: '2-digit', minute: '2-digit', second: '2-digit', timeZoneName: 'short',};

      var totalticket = 0;
      const data = OrderApiData?.data?.OrderEvent && OrderApiData?.data?.OrderEvent?.map((event:any) => {
        const categoryRoute = event?.Event?.category_id === 1 ? 'cars' : 'cash';
        var url = `/${categoryRoute}/${URIGenerator(
          event?.Event?.EventDescription[0]?.name,
          event?.Event?.id,
        )}`;

        const ticketRoute = event?.Event?.category_id === 1 ? 'CR-' : 'CA-';
        // const ticketdata = event?.EventTickets && event?.EventTickets?.map((ticket:any) => {
        //   return {
        //     ticketnumber: ticketRoute + padTicketNum(ticket.ticket_num),
        //   };
        // });

        const ticketdata = event?.EventTickets?.map((ticket:any) => ticketRoute + padTicketNum(ticket.ticket_num));


        totalticket += event?.quantity;

        const actualclosingDate = new Date(event?.Event?.end_date);
        const formattedClosingDate = actualclosingDate.toLocaleDateString('en-US', options);

        var totalperraffle = event?.Event?.price * event?.quantity;
        return {
          id: event?.event_id,
          price: event?.Event?.price,
          quantity: event?.quantity,
          total: totalperraffle,
          name: event?.Event?.EventDescription[0]?.name,
          closing_date: formattedClosingDate,
          draw_date: "",
          // draw_date: event?.Event?.end_date?.toISOString().split('T')[0],
          // draw_time: event?.Event?.end_date?.toTimeString().split(' ')[0],
          url: fullUrl+url,
          image: renderNFTImage(event?.Event),
          tickets:ticketdata
        };
      });



      // var prizenames = "";
      // var drawdate = "";
      // var drawtime = "";
      // OrderApiData?.data?.OrderEvent?.forEach((event) => {
      //   prizenames += event?.Event?.EventDescription[0]?.name + ', ';

      //   var drwdate = event?.Event?.end_date; 
      //   var date = drwdate.toISOString().split('T')[0]; 
      //   var time = drwdate.toTimeString().split(' ')[0]; 

      //   drawdate += date + ', ';
      //   drawtime += time + ', ';  

      // });
      // prizenames = prizenames.replace(/,\s*$/, "");
      // drawdate = drawdate.replace(/,\s*$/, "");
      // drawtime = drawtime.replace(/,\s*$/, "");




      console.log('API data *******', data);



      const actualDate = new Date();
      const formattedDate = actualDate.toLocaleDateString('en-US', options);


      const sendinblue: any = window.sendinblue;
      if (data) { 
        
        var discountvalue = "AED " + OrderApiData?.data?.discount_amount ? OrderApiData?.data?.discount_amount?.toFixed(2) : '0.00';

        sendinblue?.track(
          'order_completed',
          {
            "email": OrderApiData?.data?.Customer?.email,
            "FIRSTNAME": OrderApiData?.data?.Customer?.first_name
          },
          {
            "data": {
              "url":fullUrl+"/account/",
              "status": "success",
              "order_number": "INV00" + OrderApiData?.data?.id,
              "order_date": formattedDate,
              "sub_total": "AED " + OrderApiData?.data?.sub_total_amount?.toFixed(2),
              "discount": discountvalue,
              "total_price": "AED " + OrderApiData?.data?.total_amount?.toFixed(2),
              "invoice_number": "#INV00" + OrderApiData?.data?.id,
              "quantity": totalticket,
              "data" : data,
            }
          },
        ) as any;


      }

    }

  }, [OrderApiData]);



  return (
    <>
      {/* <Confetti
        width={1600}
        height={1200}
        numberOfPieces={1200}
        opacity={0.8}
        recycle={orderID > 0 && recycle}
      /> */}


      <div className="px-4 md:px-0">
        <div
          className="mt-24 md:mt-28 mb-10 h-full text-white rounded-lg mx-auto w-full px-8 py-10 sm:w-3/4 md:w-2/3"
          id="divToPrint"
        >
          <div className="flex flex-col md:flex-row items-center justify-between mb-8">
            <div className="xs:text-center md:text-left">
              <div className="greenText text-xl lg:text-2xl font-bold uppercase">Your order has been placed successfully</div>
              {/* <button onClick={handleButtonClick}>Play Audio</button> */}
            </div>

            {/* {isLoading ? null : ( */}
            <div className=" xs:text-center md:text-left">
              <div className="font-bold text-xl mb-2">INVOICE</div>
              <div className="text-sm">
                Date: {displayDate(OrderApiData?.data?.created_at)}
              </div>
              <div className="text-sm">
                Invoice: #INV00{OrderApiData?.data?.id}
              </div>
            </div>
            {/* )} */}
          </div>
          {isLoading ? null : (
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
          )}
          <ScrollArea className="w-full no-scrollbar overflow-x-scroll">
            <ScrollBar orientation="horizontal"></ScrollBar>

            <div className="min-w-[525px] mb-8">
              <div className="flex justify-between font-bold uppercase py-2">
                <div className="flex-[2] text-start">Name</div>
                <div className="flex-1 text-center">Quantity</div>
                <div className="flex-1 text-center">Price</div>
                <div className="flex-1 text-right">Total</div>
              </div>

              {isLoading ? null : (
                <div className="mt-2">
                  {OrderApiData?.data?.OrderEvent &&
                    OrderApiData?.data?.OrderEvent?.map(
                      (item: any, index: number) => (
                        <div key={index} className="flex gap-2 py-2 sm:py-4">
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
                            AED{' '}
                            {(item?.ticket_price * item?.quantity).toFixed(2)}
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
                  AED {OrderApiData?.data?.sub_total_amount?.toFixed(2)}
                </div>
              </div>

              <div className="flex justify-between items-center  mb-6">
                <div className=" mr-2">Discount:</div>
                <div className="">
                  AED{' '}
                  {OrderApiData?.data?.discount_amount
                    ? OrderApiData?.data?.discount_amount?.toFixed(2)
                    : '0.00'}
                </div>
              </div>

              <div className="flex justify-between items-center border-t-2 border-gray-300 mb-6">
                <div className=" mr-2">Total:</div>
                <div className=" font-bold text-lg">
                  AED {OrderApiData?.data?.total_amount?.toFixed(2)}
                </div>
              </div>
              <Button
                onClick={routeHandler}
                variant="clip"
                className="font-bold"
              >
                Continue Shopping
              </Button>
            </div>
          </div>
        </div>
      </div>

      <div className="relative pt-4 pb-12 px-4 md:gap-14 md:px-14 z-10 bg-card-foreground">
        <ProductSection
          class="mx-auto w-3/5 md:w-full"
          slidesToShow={3}
          center={false}
          breakpoint={[3, 2, 1.5]}
          breakpointScreens={[1350, 1050, 800]}
          title={langContent[lang.lang].Index.products.HEADING}
          type="closing"
        />
        <Glow className="absolute right-0 bottom-0 w-1/6 h-20 overflow-hidden -z-20" />
      </div>

    </>
  );
}
