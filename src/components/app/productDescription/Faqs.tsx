import React from 'react';
import Glow from '~/components/common/glow';
import {
  Accordion,
  AccordionContent,
  AccordionItem,
  AccordionTrigger,
} from '~/components/ui/accordion';

const AccordianFaqs = () => {
  return (
    <section id="AccordianFaqs" className="relative">
      <div className="mt-10 mb-10 relative pb-20 ">
        <p className="text-5xl font-black ">FAQs</p>
        <div className="border-b-4 w-16 border-primary mt-4"></div>
        <div className="relative">
          <Accordion type="single" collapsible className="w-full z-50">
            <AccordionItem value="item-1">
              <AccordionTrigger className="hover:no-underline focus-visible:text-green py-6">
                <p className="lg:text-3xl  text-xl ">Competition Rules</p>
              </AccordionTrigger>
              <AccordionContent>
                Yes. It adheres to the WAI-ARIA design pattern.
              </AccordionContent>
            </AccordionItem>
            <AccordionItem value="item-2">
              <AccordionTrigger className="hover:no-underline focus-visible:text-green py-6">
                <p className="lg:text-3xl  text-xl ">Is it styled?</p>
              </AccordionTrigger>
              <AccordionContent>
                Yes. It comes with default styles that matches the other
                components&apos; aesthetic.
              </AccordionContent>
            </AccordionItem>
            <AccordionItem value="item-3">
              <AccordionTrigger className="hover:no-underline focus-visible:text-green py-6">
                <p className="lg:text-3xl  text-xl ">Is it animated?</p>
              </AccordionTrigger>
              <AccordionContent>
                Yes. It&apos;s animated by default, but you can disable it if
                you prefer.
              </AccordionContent>
            </AccordionItem>
            <AccordionItem value="item-4">
              <AccordionTrigger className="hover:no-underline focus-visible:text-green py-6">
                <p className="lg:text-3xl  text-xl ">When is the draw?</p>
              </AccordionTrigger>
              <AccordionContent>
                Yes. It&apos;s animated by default, but you can disable it if
                you prefer.
              </AccordionContent>
            </AccordionItem>
          </Accordion>
          {/* <Glow className=" absolute  -bottom-[100px] -right-16  p-2   w-1/5 h-[350px] " /> */}
        </div>
      </div>
    </section>
  );
};

export default AccordianFaqs;
