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

const ProductDetail = () => {
  const router = useRouter();
  const { lang } = useSelector((state: RootState) => state.layout);
  const id = Number(router.query.id);
  const { data, isLoading }: any = trpc.event.getEventsById.useQuery(
    { id: id, lang_id: lang.lang_id },
    {
      refetchOnWindowFocus: false,
    },
  );

  const comp_detail: any = data?.data.EventDescription[0]?.comp_details;
  const Faqs: any = data?.data.CMS;
  return (
    <div>
      <Tabs />
      <div id="BuyTickets" className="px-4 md:px-14">
        <ImageSlider
          data={data?.data}
          ticketPurchased={data?.ticketPurchased}
        />
        <div>
          <EntiresDetail />
          <VideoSection />
        </div>
        <div></div>
      </div>
      <LiveDraw />

      <div className="relative px-4 md:px-14  pb-10">
        <div className="relative  z-10 ">
          {comp_detail ? <CompititionDetail data={data?.data} /> : <></>}
          {Faqs ? <AccordianFaqs data={data?.data} /> : <></>}
        </div>
        <Glow className="absolute bottom-0 -right-16   p-2   w-1/5 h-[80px]   " />

        {/* <Glow className=" absolute   bottom-0 -right-16  w-1/5 h-[150px] overflow-x-hidden" /> */}
      </div>
      {/* <div className="mb-10"></div> */}
      <LoadingDialog open={isLoading} text={'Loading data...'} />
    </div>
  );
};

export default ProductDetail;
