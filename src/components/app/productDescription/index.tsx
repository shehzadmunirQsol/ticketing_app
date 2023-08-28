import React from 'react';
import ImageSlider from './ImageSlider';
import EntiresDetail from './EntiresDetail';
import VideoSection from './VideoSection';
import LiveDraw from './LiveDraw';
import CompititionDetail from './CompititionDetail';
import Tabs from './Tabs';
import AccordianFaqs from './Faqs';
import LoginSignup from '../auth/LoginSignup';
import { useRouter } from 'next/router';
import { trpc } from '~/utils/trpc';

const ProductDetail = () => {
  const router = useRouter();
  const id = Number(router.query.id);
  const { data, refetch } = trpc.event.getEventsById.useQuery(
    { id },
    {
      refetchOnWindowFocus: false,
      onSuccess: () => {
        console.log({ data });
        // setCarSlider(BannerApiData || []);
      },
      // enabled: user?.id ? true : false,
    },
  );

  console.log(id, typeof id, 'i am id work');
  console.log(data?.data, 'i am data work');
  return (
    <div>
      <Tabs />
      <ImageSlider data={data?.data} />
      <div className="lg:px-10 md:px-10 px-6">
        <EntiresDetail />
        <VideoSection />
      </div>
      <LiveDraw />
      <div className="lg:px-10 md:px-10 px-6 ">
        <CompititionDetail />
        <AccordianFaqs />
      </div>
    </div>
  );
};

export default ProductDetail;
