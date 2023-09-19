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

const ProductDetail = () => {
  const router = useRouter();
  const { lang } = useSelector((state: RootState) => state.layout);
  const id = Number(router.query.id);
  const { data,isLoading } :any= trpc.event.getEventsById.useQuery(
    { id: id, lang_id: lang.lang_id },
    {
      refetchOnWindowFocus: false,
      onSuccess: () => {
        console.log({ data });
        // setCarSlider(BannerApiData || []);
      },
      // enabled: user?.id ? true : false,
    },
  );


  const comp_detail:any = data?.data.EventDescription[0]?.comp_details;
  const Faqs: any = data?.data.CMS;
  console.log(Faqs,"FaqsFaqsFaqs")
  console.log(comp_detail, 'comp_detail');
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
        {comp_detail ? <CompititionDetail data={data?.data} /> : <></>}
        {Faqs ? <AccordianFaqs data={data?.data} /> : <></>}
      </div>
      <LoadingDialog open={isLoading} text={'Loading data...'} />
    </>
  );
};

export default ProductDetail;
