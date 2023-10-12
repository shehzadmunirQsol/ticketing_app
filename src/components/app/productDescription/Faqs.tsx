import React from 'react';
import Glow from '~/components/common/glow';
import {
  Accordion,
  AccordionContent,
  AccordionItem,
  AccordionTrigger,
} from '~/components/ui/accordion';
import langContent from '~/locales';
import parse from 'html-react-parser';
import { RootState } from '~/store/store';
import { useSelector } from 'react-redux';

const eventFaqs = `
<nav class="accordion w-full   arrows">
<div class="mt-4">

    <input type="radio" name="accordion" id="cb1" />
    <section class="box border-b border-t   border-lightColorBorder hover:border-b-primary shadow-lg hover:shadow-2xl  pr-4 overflow-hidden">
      <label class="box-title  lg:text-3xl py-4 text-xl  font-bold text-white w-full flex items-center justify-between h-full" for="cb1"><p>How is the winner chosen?</p>  <i class="fas fa-chevron-down icon-class"></i></label>
      <label class="box-close" for="acc-close"></label>
      <div class="box-content text-base mt-4 text-grayColor pb-4">Your ticket number(s) will be shown as soon as your order is confirmed and will be available under <span class="text-primary font-bold">&lsquo;My Account&rsquo;</span> and in your email confirmation.</div>
    </section>
    <input type="radio" name="accordion" id="cb2" />
    <section class="box border-b border-lightColorBorder hover:border-b-primary shadow-lg hover:shadow-2xl pr-4 overflow-hidden">
      <label class="box-title  py-4 lg:text-3xl text-xl font-bold text-white w-full flex items-center justify-between " for="cb2"><p>What if a competition does not sell out?</p>  <i class="fas fa-chevron-down icon-class"></i></label>
      <label class="box-close" for="acc-close"></label>
      <div class="box-content text-base mt-4 text-grayColor pb-4">Your ticket number(s) will be shown as soon as your order is confirmed and will be available under <span class="text-primary font-bold">&lsquo;My Account&rsquo;</span> and in your email confirmation.</div>
    </section>
    <input type="radio" name="accordion" id="cb3" />
    <section class="box border-b border-lightColorBorder hover:border-b-primary shadow-lg hover:shadow-2xl  pr-4 overflow-hidden">
      <label class="box-title  py-4 lg:text-3xl text-xl font-bold text-white w-full flex items-center justify-between " for="cb3"><p>Is ‘Winnar’ a scam?</p>  <i class="fas fa-chevron-down icon-class"></i></label>
      <label class="box-close" for="acc-close"></label>
      <div class="box-content text-base mt-4 text-grayColor pb-4">Your ticket number(s) will be shown as soon as your order is confirmed and will be available under <span class="text-primary font-bold">&lsquo;My Account&rsquo;</span> and in your email confirmation.</div>
    </section>
    <input type="radio" name="accordion" id="cb4" />
    <section class="box border-b  border-lightColorBorder hover:border-b-primary shadow-lg hover:shadow-2xl  pr-4 overflow-hidden">
      <label class="box-title  py-4 lg:text-3xl text-xl font-bold text-white w-full flex items-center justify-between " for="cb4"><p>Why am I receiving communications from Winnar?</p>  <i class="fas fa-chevron-down icon-class"></i></label>
      <label class="box-close" for="acc-close"></label>
      <div class="box-content text-base mt-4 text-grayColor pb-4">Your ticket number(s) will be shown as soon as your order is confirmed and will be available under <span class="text-primary font-bold">&lsquo;My Account&rsquo;</span> and in your email confirmation.</div>
    </section>
    <input type="radio" name="accordion" id="acc-close" />
    </div>
  </nav>
`;

let carouselHeading: any = {};
const findElementsWithAttribute = (node: any) => {
  if (node.type === 'tag') {
    const shortcode = node?.attribs;
    console.log(shortcode, 'shortcode');
    if (shortcode?.data === 'main-carousel-heading') {
      carouselHeading = JSON.parse(node?.children[0]?.data) as any;
      console.log(carouselHeading, 'carouselHeading');
      return <></>;
    }

    return node;
  }
};
const AccordianFaqs = ({ data }: any) => {
  const { lang } = useSelector((state: RootState) => state.layout);

  const dataCode: any = data?.CMS?.CMSDescription[0]?.content;
  const reactElementsForFAQs = parse(dataCode || '', {
    replace: (node: any) => findElementsWithAttribute(node),
  });
  return (
    <section id="AccordianFaqs" className="relative">
      <>
        <div className="mt-10 ">
          <p className="text-5xl font-black">
            {' '}
            {langContent[lang.lang].ProductDetail.faqs.HEADING}
          </p>
          <div className="border-b-4 w-16 border-primary mt-4"></div>
        </div>
        <div className="mt-10 mb-10 relative pb-20 ">
          {reactElementsForFAQs}
        </div>
      </>
    </section>
  );
};

export default AccordianFaqs;
