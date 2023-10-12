import React from 'react';
import EngineIcon from '~/public/assets/icons/engine.svg';
import PowerIcon from '~/public/assets/icons/power.svg';
import KmsIcon from '~/public/assets/icons/kms.svg';
import YearIcon from '~/public/assets/icons/year.svg';
import Image from 'next/image';
import langContent from '~/locales';
import { RootState } from '~/store/store';
import { useSelector } from 'react-redux';

function Highlights(props: any) {
  const { meta } = props;
  const { lang } = useSelector((state: RootState) => state.layout);

  return (
    <div className="space-y-8">
      <h2 className="lg:text-5xl md:text-4xl text-2xl   font-black uppercase">
        {langContent[lang.lang].ProductDetail.highlights.title}
      </h2>
      <div className="border-b-4 w-16 border-primary mt-4 mb-14"></div>

      <div className="flex items-center space-x-4 justify-between no-scrollbar overflow-x-scroll">
        {highlights.map((highlight) => (
          <div
            key={highlight.key}
            className="flex flex-col rtl:ml-4 gap-3 md:gap-6 md:items-center p-4  min-w-[150px] w-full border-2 border-primary rounded-xl"
          >
            <Image
              src={highlight.icon}
              width={100}
              height={100}
              alt={highlight.title}
              className="w-12 h-12 mx-auto md:w-20 md:h-20"
            />
            <div className="">
              <h3 className="text-3xl md:text-4xl">{meta[highlight.key]}</h3>
              <p className="text-xl md:text-2xl text-[#9D9D9D]">
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
    title: 'Kms',
    icon: KmsIcon,
    key: 'kms',
  },
  {
    title: 'Year',
    icon: YearIcon,
    key: 'year',
  },
] as const;
