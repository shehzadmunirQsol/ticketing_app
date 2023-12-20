import React, { useEffect, useState } from 'react';
import { useSelector } from 'react-redux';
import { RootState } from '~/store/store';
import { trpc } from '~/utils/trpc';
import { renderNFTImage } from '~/utils/helper';
import Link from 'next/link';
import Slider from 'react-slick';
import DubaiETlogo from '~/public/assets/ded.png';
import NextImage from '~/components/ui/img';

function BannerSlider() {
  const { lang } = useSelector((state: RootState) => state.layout);
  const [carSliderNew, setCarSliderNew] = useState<Array<any>>([]);

  const slider = React.useRef(null);
  const mainSlider = {
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

  const { data: BannerApiData } = trpc.settings.get_banner.useQuery(
    initialOrderFilters,
    {
      refetchOnWindowFocus: false,
    },
  );

  useEffect(() => {
    if (BannerApiData?.data) {
      setCarSliderNew(BannerApiData.data);
    }
  }, [BannerApiData?.data]);

  return (
    <div className="herobanner px-2 md:px-12">
      <div className="leftbx">
        <h1 className="uppercase">Are you the next lucky winnar?</h1>
        {/* <h3 className="uppercase">+ 4000 AED CASH</h3> */}
        <p>Drive home your dream car at a fraction of the price!</p>
        <h4 className="uppercase">WIN SUNDAY 8 PM</h4>
        <Link href="/cars" className="winbtn font-sans desktopbx">
          ENTER NOW
        </Link>
        <div className="dblogo desktopbx">
          <NextImage src={DubaiETlogo} alt="banner image" />
        </div>
      </div>

      <div className="rightbx">
        {carSliderNew ? (
          <Slider ref={slider} {...mainSlider} className="slidernavdown">
            {carSliderNew.map((data, key) => {
              console.log("dsfdssdfs", data);
              return (
                <div key={key} className="item">
                  <Link href={data.link}>
                    <div className="imgbx">
                      <NextImage
                        src={renderNFTImage(data)}
                        alt="banner image"
                        quality={100}
                        width={100}
                        height={100}
                      />
                    </div>
                    <div className="bannertext">
                      <p>{data.title}</p>
                      <div className="ddate" dangerouslySetInnerHTML={{ __html: data.description }} />
                      {/* <div className="ddate">
                      <span>Draw Date: 30th Dec, 2023</span> <Link href={data.link}>Register Now <i className="fa-solid fa-arrow-right"></i></Link>
                      </div> */}
                    </div>
                  </Link>
                </div>
              );
            })}
          </Slider>
        ) : null}
      </div>

      <div className="leftbx mobilebx">
        <Link href="/cars" className="winbtn font-sans">
          ENTER NOW
        </Link>
        <div className="dblogo">
          <NextImage src={DubaiETlogo} alt="banner image" />
        </div>
      </div>
    </div>
  );
}

export default BannerSlider;
