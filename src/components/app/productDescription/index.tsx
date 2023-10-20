import React from 'react';
import ImageSlider from './ImageSlider';
import EntiresDetail from './EntiresDetail';
import VideoSection from './VideoSection';
import LiveDraw from './LiveDraw';
import CompititionDetail from './compitition_detail';
import Tabs from './Tabs';
import AccordianFaqs from './Faqs';
import { useRouter } from 'next/router';
import { trpc } from '~/utils/trpc';
import { useSelector } from 'react-redux';
import { RootState } from '~/store/store';
import { LoadingDialog } from '~/components/common/modal/loadingModal';
import Glow from '~/components/common/glow';
import { URIDecoder } from '~/utils/helper';
import Highlights from './highlights';

const ProductDetail = () => {
  const router = useRouter();
  const counterRef = React.useRef<any>(null);
  const { lang } = useSelector((state: RootState) => state.layout);
  const { id } = URIDecoder(router?.query?.id ?? '');
  const { data, isLoading } = trpc.event.getEventsById.useQuery(
    { id: Number(id), lang_id: lang.lang_id },
    {
      refetchOnWindowFocus: false,
    },
  );

  const comp_detail: any = data?.data?.EventDescription[0]?.comp_details;
  const Faqs: any = data?.data?.CMS;

  const isMeta = data?.data?.category_id === 1;
  const meta =
    isMeta && data?.data?.meta && data?.data?.meta?.includes('engine')
      ? JSON.parse(data?.data?.meta as string)
      : '';

  React.useEffect(() => {
    counter();
  }, []);
  /**Counter */

  const counter = () => {
    // Set the date we're counting down to
    const countDownDate: any = 1698782400000; //new Date("Oct 31, 2023 24:00:00").getTime();

    // Update the count down every 1 second
    setInterval(() => {
      // Get todays date and time
      const now: any = new Date().getTime();

      // Find the distance between now an the count down date
      const distance: any = countDownDate - now;

      // Time calculations for days, hours, minutes and seconds
      const days: any = Math.floor(distance / (1000 * 60 * 60 * 24));
      const hours: any = Math.floor(
        (distance % (1000 * 60 * 60 * 24)) / (1000 * 60 * 60),
      );
      const minutes: any = Math.floor(
        (distance % (1000 * 60 * 60)) / (1000 * 60),
      );
      const seconds: any = Math.floor((distance % (1000 * 60)) / 1000);

      // Output the result in an element with id="demo"
      if (counterRef) {
        counterRef.current.innerHTML =
          '<div class="flex flex-row md:flex-col md:justify-start md:items-start space-x-2 md:space-y-4"><div class="flex flex-col md:flex-row justify-center md:row md:items-start"><span class="text-lg font-normal md:text-2xl">COMPETITION</span><span class="text-2xl font-normal">CLOSES IN</span></div><div class="flex space-x-1 md:space-x-4 flex-1 md:flex-none"><p class="timer-box flex flex-col gap-3 md:gap-6 items-center py-4 px-1 md:p-4 min-w-fit w-full border-2 border-primary rounded-xl"><span class="text-3xl sm:text-5xl font-normal text-primary">' +
          days +
          '</span><span class="text-xs sm:text-base">' +
          'Days' +
          '</span></p> ' +
          '<p class="timer-box flex flex-col gap-3 md:gap-6 items-center py-4 px-1 md:p-4 min-w-fit w-full border-2 border-primary rounded-xl"><span class="text-3xl sm:text-5xl font-normal text-primary">' +
          hours +
          '</span><span class="text-xs sm:text-base">' +
          'Hours' +
          '</span></p> ' +
          '<p class="timer-box flex flex-col gap-3 md:gap-6 items-center py-4 px-1 md:p-4 min-w-fit w-full border-2 border-primary rounded-xl"><span class="text-3xl sm:text-5xl font-normal text-primary">' +
          minutes +
          '</span><span class="text-xs sm:text-base">' +
          'Mins' +
          '</span></p> ' +
          '<p class="timer-box flex flex-col gap-3 md:gap-6 items-center py-4 px-1 md:p-4 min-w-fit w-full border-2 border-primary rounded-xl"><span class="text-3xl sm:text-5xl font-normal text-primary">' +
          seconds +
          '</span><span class="text-xs sm:text-base">' +
          'Secs' +
          '</span></p></div></div>';
      }
    }, 1000);
  };

  return (
    <div className="bg-background">
      <Tabs data={data?.data} comp_detail={comp_detail} />
      <div id="BuyTickets" className="px-4 md:px-14 ">
        <ImageSlider
          data={data?.data}
          ticketPurchased={data?.ticketPurchased}
        />
        <div ref={counterRef}></div>
        <div>
          <EntiresDetail data={data?.data} />
          {isMeta ? <Highlights meta={meta} /> : null}
          <VideoSection data={data?.data} />
        </div>
      </div>
      <LiveDraw data={data?.data} />

      <div className="relative px-4 md:px-14  ">
        <div className="relative z-10 ">
          {comp_detail ? <CompititionDetail data={data?.data} /> : <></>}
          {Faqs && Faqs?.is_enabled ? (
            <AccordianFaqs data={data?.data} />
          ) : (
            <></>
          )}
        </div>
        <Glow className="absolute bottom-0 -right-16   p-2   w-1/5 h-[80px]   " />
      </div>
      <LoadingDialog open={isLoading} text={'Loading data...'} />
    </div>
  );
};

export default ProductDetail;
