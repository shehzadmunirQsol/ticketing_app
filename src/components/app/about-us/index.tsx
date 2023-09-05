import React, { useState } from 'react';
import BannerTitle from '~/components/common/banner_title';
import BackgroundImage from '~/public/assets/account.png';
import AccountView from '~/components/app/account/account-view';
import { trpc } from '~/utils/trpc';
import { useToast } from '~/components/ui/use-toast';
import { useRouter } from 'next/router';

const AboutUs = () => {
  const { toast } = useToast();
  const router = useRouter();

  const [counter, setCounter] = useState(0);

  const { data: cms, isLoading } = trpc.cms.getCmsAboutUs.useQuery(
    {},
    {
      refetchOnWindowFocus: false,
    },
  );
  console.log(cms, 'customer');

  return (
    <div className='mt-96'>
      {cms?.map((item:any, i) => {
        console.log(item,"items found")
        return (
          <div key={i} dangerouslySetInnerHTML={{ __html: item.CMSDescription[0]?.content }}>
          </div>
        );
      })}
    </div>
  );
};

export default AboutUs;
