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

const ProductDetail = () => {
  const router = useRouter();
  const { lang } = useSelector((state: RootState) => state.layout);
  const id = Number(router.query.id);
  const { data } = trpc.event.getEventsById.useQuery(
    { id: id, lang_id: lang.lang_id },
    {
      refetchOnWindowFocus: false,
    },
  );

  return (
    <>
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

      <div className="px-4 md:px-14">
        <CompititionDetail data={data?.data} />
        <AccordianFaqs />
      </div>
    </>
  );
};

export default ProductDetail;
