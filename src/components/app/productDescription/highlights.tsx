import React from 'react';
import EngineIcon from '~/public/assets/icons/engine.svg';
import PowerIcon from '~/public/assets/icons/power.svg';
import KmsIcon from '~/public/assets/icons/kms.svg';
import YearIcon from '~/public/assets/icons/year.svg';
import TransmissionIcon from '~/public/assets/icons/transmission.svg';
import langContent from '~/locales';
import { RootState } from '~/store/store';
import { useSelector } from 'react-redux';
import NextImage from '~/components/ui/img';

function Highlights(props: any) {
  const { meta } = props;
  const { lang } = useSelector((state: RootState) => state.layout);

  return (
    <div className="">
      <div className="text-basic md:text-lg mb-2 text-white mt-5">
        {langContent[lang.lang].ProductDetail.highlights.title}
      </div>

      <div className="flex items-center space-x-2 md:space-x-4 justify-between no-scrollbar overflow-x-scroll">
        {highlights.map((highlight) => (
          <div
            key={highlight.key}
            className="highlightbx flex flex-col rtl:ml-4 gap-1 items-center p-2 min-w-fit w-full border border-primary rounded-lg text-center"
          >
            <NextImage
              src={highlight.icon}
              width={100}
              height={100}
              alt={highlight.title}
              className="mx-auto w-6 h-6"
            />
            <div className="">
              <h3 className="text-sm md:text-basic text-white">{meta[highlight.key]}</h3>
              <p className="text-xs md:text-xs text-[#9D9D9D]">
                {
                  langContent[lang.lang]?.ProductDetail?.highlights[
                    highlight?.key
                  ] as any
                }
              </p>
            </div>
          </div>
        ))}
      </div>
    </div>
  );
}

export default Highlights;

const highlights = [
  {
    title: 'Engine',
    icon: EngineIcon,
    key: 'engine',
  },
  {
    title: 'Power',
    icon: PowerIcon,
    key: 'power',
  },
  {
    title: 'Transmission',
    icon: TransmissionIcon,
    key: 'kms',
  },
  {
    title: 'Year',
    icon: YearIcon,
    key: 'year',
  },
] as const;
