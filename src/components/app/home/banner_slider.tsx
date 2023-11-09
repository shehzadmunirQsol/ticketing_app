import React, { useEffect, useRef, useState } from 'react';
import Image from 'next/image';
import { Button } from '~/components/ui/button';
import { useSelector } from 'react-redux';
import { RootState } from '~/store/store';
import { trpc } from '~/utils/trpc';
import { renderNFTImage } from '~/utils/helper';
import Link from 'next/link';
import { useRouter } from 'next/router';
import langContent from '~/locales';
import Slider from "react-slick";
import DubaiETlogo from '~/public/assets/dubai-eq-logo.png';

function BannerSlider() {
  const { lang } = useSelector((state: RootState) => state.layout);
  const [carSliderNew, setCarSliderNew] = useState<Array<any>>([]);

  const router = useRouter();

  const slider = React.useRef(null);
  var mainSlider = {
    slidesToShow: 1,
    autoplay: true,
    autoplaySpeed: 5000,
    speed: 1000,
    dots: false,
    arrows: true,
  };

  const initialOrderFilters: any = {
    lang_id: lang.lang_id,
    group: 'BANNER',
    is_enabled: true,
    rows: 4,
    first: 0,
    page: 0,
  };

  const { data: BannerApiData, isSuccess } = trpc.settings.get_banner.useQuery(
    initialOrderFilters,
    {
      refetchOnWindowFocus: false,
      // enabled: user?.id ? true : false,
    },
  );

  useEffect(() => {
    if (BannerApiData?.data) {
      setCarSliderNew(BannerApiData.data);
    }
  }, [BannerApiData?.data]);

  // useEffect(() => {
  //   console.log(carSliderNew[0])
  //   console.log(carSliderNew[0].value)
 
  // }, [carSliderNew]);


  return (



<div className="herobanner px-2 md:px-12">

  <div className="leftbx">
        <h1>WIN THIS NISSAN PATROL NISMO</h1>
        <h3>+ 4000 AED CASH</h3>
        <p>Get your dream car at a fraction of the price</p>
        <h4>WIN SUNDAY 8 PM</h4>
        <Link href="" className="winbtn font-sans desktopbx">ENTER NOW</Link>
        <div className="dblogo desktopbx"><Image src={DubaiETlogo} alt="banner image" /></div>
  </div>
 

  <div className="rightbx">
  {
    carSliderNew ?
      <Slider ref={slider} {...mainSlider} className="slidernavdown">

{carSliderNew.map((data, key) => {
                    return <div className="item">
                        <Link href={data.link}>
                          <div className="imgbx">
                              <Image
                              src={renderNFTImage(data)}
                              alt="banner image"
                              quality={100}
                              width={100}
                              height={100}
                              />
                            </div>
                            <div className="bannertext">
                              <p>
                              {data.title}
                              </p>
                            </div>
                        </Link>
                  </div>
            })
          }
      </Slider>
  :
  null
}
  </div>

  <div className="leftbx mobilebx">
        <Link href="" className="winbtn font-sans">ENTER NOW</Link>
        <div className="dblogo"><Image src={DubaiETlogo} alt="banner image" /></div>
  </div>

</div>

  );
}

export default BannerSlider;
