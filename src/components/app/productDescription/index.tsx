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
  console.log(Faqs, 'Faqs');

  const isMeta = data?.data?.category_id === 1;
  const meta =
    isMeta && data?.data?.meta && data?.data?.meta?.includes('engine')
      ? JSON.parse(data?.data?.meta as string)
      : '';

  return (
    <div className="bg-background">
      <Tabs
        isFAQ={!!Faqs?.is_enabled}
        isCompetition={!!comp_detail}
        data={data?.data}
        comp_detail={comp_detail}
      />
      <div id="BuyTickets" className="px-4 md:px-14 ">
        <ImageSlider
          data={data?.data}
          ticketPurchased={data?.ticketPurchased}
        />
        <div>
          <EntiresDetail data={data?.data} />
          {isMeta ? <Highlights meta={meta} /> : null}
          <VideoSection data={data?.data} />
        </div>
      </div>
      <LiveDraw data={data?.data} />

      <div className="relative px-4 md:px-14  ">
        <div className="relative z-10 ">
          {comp_detail ? <CompititionDetail data={data?.data} /> : null}
          {Faqs?.is_enabled && Faqs?.CMSDescription?.length > 0 ? (
            <AccordianFaqs data={data?.data} />
          ) : null}
        </div>
        <Glow className="absolute bottom-0 -right-16   p-2   w-1/5 h-[80px]   " />
      </div>
      <LoadingDialog open={isLoading} text={'Loading data...'} />
    </div>
  );
};

export default ProductDetail;
