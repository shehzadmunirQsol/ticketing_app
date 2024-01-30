import dynamic from 'next/dynamic';
import React from 'react';
import { prisma } from '~/server/prisma';
import parse from 'html-react-parser';
import AboutCarousel from '~/components/app/about/about_carousel';
import jqeury from 'jquery';
import { useSelector } from 'react-redux';
import { RootState } from '~/store/store';
import { Lang } from '~/store/reducers/layout';

export async function getStaticPaths() {
  const response: any = await prisma.cMS.findMany({
    select: {
      id: true,
      slug: true,
    },
    where: {
      is_enabled: true,
      is_deleted: false,
    },
  });
  const filterData = response?.filter(
    (item: any) => item?.slug && item?.slug !== '',
  );
  const paths = filterData?.map((post: any) => ({
    params: { id: post?.slug },
  }));

  return { paths, fallback: true };
}
export async function getStaticProps({ params }: any) {
  const response = await prisma.cMS.findFirst({
    where: {
      slug: params?.id,
      is_enabled: true,
    },

    select: {
      id: true,
      slug: true,
      type: true,
      CMSDescription: {
        orderBy: {
          lang_id: 'asc',
        },
        select: {
          content: true,
        },
      },
    },
  });

  return { props: { storeBlogsData: response }, revalidate: 10 };
}

const CmsFunc = dynamic(() => import('~/components/app/cms/index'), {
  ssr: true,
});

const AboutUsContent = ` <div className='bg-background'>

<div className="w-full relative !h-[450px] md:!h-[550px] text-center">
    <div className="relative w-full h-full">
      <Image
        src="https://media.winnar.com/upload/about-page-background.png"
        alt="/"
        fill
        quality={100}
        className=" relative object-cover  w-full h-full block bg-black/50"
      />
      <div className="absolute h-[35px] w-full text-center top-[50%] flex items-center">
        <div className="w-full text-center ">
          <p className=" text-white drop-shadow-2xl text-center w-full text-4xl  tracking-wide lg:text-5xl uppercase font-[900]">
            PRESTIGE CAR
          </p>
          <p className="relative h-[35px] -top-[50%]   mt-3 text-white drop-shadow-2xl text-center w-full text-4xl  lg:text-5xl   uppercase font-[900]">
            RAFFLES
          </p>
        </div>
      </div>
    </div>
    </div>
    <div className="w-full z-50 -mt-24">
      <div className="bg-primary lg:w-[400px] md:w-[400px]  px-8 py-20 aboutus-card   mr-10 lg:ml-20 ml-5  mb-20">
        <p className="font-black lg:text-5xl md:text-2xl text-xl text-black">Our Mission</p>
        <p className="text-background pt-5 text-sm">
          Winnar, the prestige car ra e company that o ers car enthusiasts the
          thrilling opportunity to win their coveted dream car and many other
          prizes. Through our captivating experiences and philanthropic
          initiatives, we aim to create memorable moments that extend beyond
          the thrill of winning! Join Winnar.com today and get ready to drive
          away in the car of your dreams.
        </p>
      </div>
    </div>


<div className="absolute top-[610px] right-64 px-8 xl:block hidden">
  <svg
    width="474"
    height="895"
    viewBox="0 0 674 1095"
    fill="none"
    xmlns="http://www.w3.org/2000/svg"
  >
    <path
      d="M1.11379 547.5L486.887 1093.73H819.886L334.113 547.5H1.11379Z"
      stroke="url(#paint0_linear_190_685)"
    />
    <path
      d="M1.11379 546.728L486.887 0.5H819.886L334.113 546.728H1.11379Z"
      stroke="url(#paint1_linear_190_685)"
    />
    <defs>
      <linearGradient
        id="paint0_linear_190_685"
        x1="706"
        y1="1094.23"
        x2="220"
        y2="531.228"
        gradientUnits="userSpaceOnUse"
      >
        {/* <stop stop-opacity="0" /> */}
        {/* <stop offset="1" stop-color="#454545" /> */}
      </linearGradient>
      <linearGradient
        id="paint1_linear_190_685"
        x1="706"
        y1="-1.26162e-05"
        x2="220"
        y2="563"
        gradientUnits="userSpaceOnUse"
      >
        {/* <stop stop-opacity="0" /> */}
        {/* <stop offset="1" stop-color="#454545" /> */}
      </linearGradient>
    </defs>
  </svg>
</div>
  <div data="main-carousel">
  [
    {
        "img":"https://media.winnar.com/upload/founder.png",
        "heading":"Scott L. Hughes",
        "text":"Co-Founder",
        "hoverhead":"Scott L. Hughes",
        "hoverpera":"Co-Founder",
        "hoverdesc":"Aenean vulputate eleifend tellus Aenean leo ligula porttitor eu consequat vitae eleifend ac enim. Aliquam lorem ante dapibus in viverra quis feugiat a tellus Phasellus viverra nulla ut metus varius laoreet Quisque rutrum"
    },
    {
        "img":"https://media.winnar.com/upload/founder-1.png",
        "heading":"Scott L. Hughes",
        "text":"Founder",
        "hoverhead":"Scott L. Hughes",
        "hoverpera":"Founder",
        "hoverdesc":"Aenean vulputate eleifend tellus Aenean leo ligula porttitor eu consequat vitae eleifend ac enim. Aliquam lorem ante dapibus in viverra quis feugiat a tellus Phasellus viverra nulla ut metus varius laoreet Quisque rutrum"
    },
    {
        "img":"https://media.winnar.com/upload/founder-2.png",
        "heading":"Eric M. Carroll",
        "text":"Founder",
        "hoverhead":"Scott L. Hughes",
        "hoverpera":"Founder",
        "hoverdesc":"Aenean vulputate eleifend tellus Aenean leo ligula porttitor eu consequat vitae eleifend ac enim. Aliquam lorem ante dapibus in viverra quis feugiat a tellus Phasellus viverra nulla ut metus varius laoreet Quisque rutrum"
    },
    {
        "img":"https://media.winnar.com/upload/founder-3.png",
        "heading":"Ronnie D. Blake",
        "text":"Founder",
        "hoverhead":"Scott L. Hughes",
        "hoverpera":"Founder",
        "hoverdesc":"Aenean vulputate eleifend tellus Aenean leo ligula porttitor eu consequat vitae eleifend ac enim. Aliquam lorem ante dapibus in viverra quis feugiat a tellus Phasellus viverra nulla ut metus varius laoreet Quisque rutrum"
        
    },
    {
        "img":"https://media.winnar.com/upload/founder.png",
        "heading":"Marvin L. Orr",
        "text":"Founder",
        "hoverhead":"Scott L. Hughes",
        "hoverpera":"Founder",
        "hoverdesc":"Aenean vulputate eleifend tellus Aenean leo ligula porttitor eu consequat vitae eleifend ac enim. Aliquam lorem ante dapibus in viverra quis feugiat a tellus Phasellus viverra nulla ut metus varius laoreet Quisque rutrum"
        
    },
    {
      "img":"https://media.winnar.com/upload/founder.png",
      "heading":"Ronnie D. Blake",
      "text":"Founder",
      "hoverhead":"Scott L. Hughes",
      "hoverpera":"Founder",
      "hoverdesc":"Aenean vulputate eleifend tellus Aenean leo ligula porttitor eu consequat vitae eleifend ac enim. Aliquam lorem ante dapibus in viverra quis feugiat a tellus Phasellus viverra nulla ut metus varius laoreet Quisque rutrum"
  }
]
</div>
<div className="w-full py-32 lg:px-14 md:px-14 px-4 z-40 text-center bg-Image">
    <div className=" text-center flex flex-col lg:flex-row gap-x-20 justify-center items-start font-sans lg:mx-auto">
      <p className=" text-white drop-shadow-2xl text-center  font-black text-2xl  lg:text-4xl lg:w-fit sm:mb-5">
      OUR UVP
      </p>
      <div className=" font-normal max-w-2xl  w-fit">
        <p className=" text-grayColor drop-shadow-2xl text-start text-xl  ">
        At Winnar, we offer discerning individuals an exclusive opportunity to win their coveted dream car from our meticulously curated collection of performance, luxury, and prestige brands.
        </p>
        <p className=" text-grayColor drop-shadow-2xl text-start text-md   pt-10">
          What sets us apart is our unwavering focus on transparency,
          fairness, and excitement. Unlike traditional car purchasing
          methods, Winnar.com provides an electrifying platform where
          every participant is empowered to drive away in the car of their
          dreams. With our transparent selection process, participants can
          be confident that their chances are fair, and the thrill of
          winning is unparalleled. Join us today to experience the epitome
          of automotive aspirations and be a part of an exceptional ra e
          journey.
        </p>
      </div>
    </div>
</div>

<div className="w-full relative h-[250px] lg:h-[550px] z-40 text-center">
  <Image
    src="https://media.winnar.com/upload/about-background-2.png"
    alt="/"
    fill
    quality={100}
    className=" object-cover w-full  h-full block bg-black/50"
  />
  <div className="absolute h-[35px] w-full text-center top-[50%] flex items-center">
    <div className="w-full text-center ">
      <p className=" text-white drop-shadow-2xl text-center w-full text-xl  text-xl">
        Get your dream car at a fraction of the price!
      </p>
      <p className="relative h-[35px] -top-[50%]   mt-6 text-white drop-shadow-2xl text-center w-full text-2xl  lg:text-4xl font-[900]">
        WIN SUNDAY 8 PM
      </p>
      <a href="/cars">
          <Button
            className=" px-8 mt-6 py-2  bg-primary clip-style text-black font-sans font-[900] text-xl"
            variant="default"
          >
          Enter Now
          </Button>
      </a>
      </div>
  </div>
</div>
</div>`;

const AboutUsContentTwo = `
<div classname="bg-background">
<div classname="w-full relative h-[750px] md:h-[650px] lg:h-[550px] text-center">
<div classname="relative w-full h-full"><img alt="/" classname=" relative object-cover  w-full h-full block bg-black/50" fill="" quality="{100}" src="https://media.winnar.com/upload/about-page-background.png" />
<div classname="absolute h-[35px] w-full text-center top-[50%] flex items-center">
<div classname="w-full text-center ">
<p classname=" text-white drop-shadow-2xl text-center w-full text-4xl  tracking-wide lg:text-5xl uppercase font-[900]">PRESTIGE CAR</p>

<p classname="relative h-[35px] -top-[50%]   mt-3 text-white drop-shadow-2xl text-center w-full text-4xl  lg:text-5xl   uppercase font-[900]">RAFFLES</p>
</div>
</div>
</div>
</div>

<div classname="w-full z-50 -mt-24">
<div classname="bg-primary lg:w-[400px] md:w-[400px]  px-8 py-20 aboutus-card   mr-10 lg:ml-20 ml-5  mb-20">
<p classname="font-black lg:text-5xl md:text-2xl text-xl text-black">Our Mission</p>

<p classname="text-background pt-5 text-sm">Winnar, the prestige car raffle company that offers car enthusiasts the thrilling opportunity to win their coveted dream car and many other prizes. Through our captivating experiences and philanthropic initiatives, we aim to create memorable moments that extend beyond the thrill of winning! Join Winnar.com today and get ready to drive away in the car of your dreams.</p>
</div>
</div>

<div classname="absolute top-[610px] right-64 px-8 xl:block hidden"><svg fill="none" height="895" viewbox="0 0 674 1095" width="474" xmlns="http://www.w3.org/2000/svg"> <path d="M1.11379 547.5L486.887 1093.73H819.886L334.113 547.5H1.11379Z" stroke="url(#paint0_linear_190_685)"></path> <path d="M1.11379 546.728L486.887 0.5H819.886L334.113 546.728H1.11379Z" stroke="url(#paint1_linear_190_685)"></path> <defs> <lineargradient gradientunits="userSpaceOnUse" id="paint0_linear_190_685" x1="706" x2="220" y1="1094.23" y2="531.228"> {/* <stop stop-opacity="0"></stop> */} {/* <stop offset="1" stop-color="#454545"></stop> */} </lineargradient> <lineargradient gradientunits="userSpaceOnUse" id="paint1_linear_190_685" x1="706" x2="220" y1="-1.26162e-05" y2="563"> {/* <stop stop-opacity="0"></stop> */} {/* <stop offset="1" stop-color="#454545"></stop> */} </lineargradient> </defs> </svg></div>
<div data="main-carousel">
[
    {
        "img":"https://media.winnar.com/upload/founder.png",
        "heading":"سكوت ل. هيوز",
        "text":"شريك مؤسس",
        "hoverhead":"سكوت ل. هيوز",
        "hoverpera":"شريك مؤسس",
        "hoverdesc":"في وينار، نقدم للأفراد المميزين فرصة حصرية للفوز بسيارة أحلامهم المرغوبة من مجموعتنا المنسقة بدقة من العلامات التجارية عالية الأداء والفخامة والهيبة."
        
    },
    {
        "img":"https://media.winnar.com/upload/founder-1.png",
        "heading":"سكوت ل. هيوز",
        "text":"مؤسس",
        "hoverhead":"سكوت ل. هيوز",
        "hoverpera":"مؤسس",
        "hoverdesc":"في وينار، نقدم للأفراد المميزين فرصة حصرية للفوز بسيارة أحلامهم المرغوبة من مجموعتنا المنسقة بدقة من العلامات التجارية عالية الأداء والفخامة والهيبة."

        
    },
    {
        "img":"https://media.winnar.com/upload/founder-2.png",
        "heading":"إريك م. كارول",
        "text":"مؤسس",
        "hoverhead":"سكوت ل. هيوز",
        "hoverpera":"مؤسس",
        "hoverdesc":"في وينار، نقدم للأفراد المميزين فرصة حصرية للفوز بسيارة أحلامهم المرغوبة من مجموعتنا المنسقة بدقة من العلامات التجارية عالية الأداء والفخامة والهيبة."

        
    },
    {
        "img":"https://media.winnar.com/upload/founder-3.png",
        "heading":"روني د. بليك",
        "text":"مؤسس",
        "hoverhead":"سكوت ل. هيوز",
        "hoverpera":"مؤسس",
        "hoverdesc":"في وينار، نقدم للأفراد المميزين فرصة حصرية للفوز بسيارة أحلامهم المرغوبة من مجموعتنا المنسقة بدقة من العلامات التجارية عالية الأداء والفخامة والهيبة."
        
    }
]
</div>


<div classname="w-full py-32 lg:px-14 md:px-14 px-4 z-40 text-center bg-Image">
<div classname=" text-center flex flex-col lg:flex-row gap-x-4 justify-center items-start font-sans lg:mx-auto">
<p classname=" text-white drop-shadow-2xl text-center  font-black text-2xl  text-xl lg:w-fit">OUR MVP</p>

<div classname=" font-normal max-w-2xl w-fit">
<p classname=" text-white drop-shadow-2xl text-justify text-sm ">At Winnar, we offer discerning individuals an exclusive opportunity to win their coveted dream car from our meticulously curated collection of performance, luxury, and prestige brands.</p>

<p classname=" text-white drop-shadow-2xl text-sm text-justify  pt-10">What sets us apart is our unwavering focus on transparency, fairness, and excitement. Unlike traditional car purchasing methods, Winnar.com provides an electrifying platform where every participant is empowered to drive away in the car of their dreams. With our transparent selection process, participants can be confident that their chances are fair, and the thrill of winning is unparalleled. Join us today to experience the epitome of automotive aspirations and be a part of an exceptional raffle journey.</p>
</div>
</div>
</div>

<div classname="w-full relative h-[250px] lg:h-[550px] z-40 text-center"><img alt="/" classname=" object-cover w-full  h-full block bg-black/50" fill="" quality="{100}" src="https://media.winnar.com/upload/about-background-2.png" />
<div classname="absolute h-[35px] w-full text-center top-[50%] flex items-center">
<div classname="w-full text-center ">
<p classname=" text-white drop-shadow-2xl text-center w-full text-xl  text-xl">Get your dream car at a fraction of the price!</p>

<p classname="relative h-[35px] -top-[50%]   mt-6 text-white drop-shadow-2xl text-center w-full text-2xl  lg:text-4xl font-[900]">WIN SUNDAY 8 PM</p>
<button classname=" px-8 mt-6 py-2  bg-primary clip-style text-black font-sans font-[900] text-xl" variant="default">Enter Now</button></div>
</div>
</div>
</div>

`;

const faqUpdatePage = `
<div class="w-full relative h-[250px] md:h-[450px]  z-40 text-center "><img alt="/" class="object-cover  w-full h-full block bg-black/50" fill="" quality="{100}" src="https://media.winnar.com/upload/faq-banner.png" />
<div class="absolute h-[35px] w-full text-center top-[50%] flex items-center">
<div class="w-full text-center">
<p class="text-white drop-shadow-2xl text-center w-full text-4xl  lg:text-5xl tracking-tighter   font-[900]">FAQs</p>
</div>
</div>
</div>

<div classname="absolute top-[40px] right-64 px-8 xl:block hidden"><svg fill="none" height="895" viewbox="0 0 674 1095" width="474" xmlns="http://www.w3.org/2000/svg"> <path d="M1.11379 547.5L486.887 1093.73H819.886L334.113 547.5H1.11379Z" stroke="url(#paint0_linear_190_685)"></path> <path d="M1.11379 546.728L486.887 0.5H819.886L334.113 546.728H1.11379Z" stroke="url(#paint1_linear_190_685)"></path> <defs> <lineargradient gradientunits="userSpaceOnUse" id="paint0_linear_190_685" x1="706" x2="220" y1="1094.23" y2="531.228"> {/* <stop stop-opacity="0"></stop> */} {/* <stop offset="1" stop-color="#454545"></stop> */} </lineargradient> <lineargradient gradientunits="userSpaceOnUse" id="paint1_linear_190_685" x1="706" x2="220" y1="-1.26162e-05" y2="563"> {/* <stop stop-opacity="0"></stop> */} {/* <stop offset="1" stop-color="#454545"></stop> */} </lineargradient> </defs> </svg></div>

<div class="relative w-full flex flex-col gap-8  lg:px-14 md:px-14 px-4 mt-16 mb-24">
<div>
<p class="lg:text-5xl md:text-4xl text-2xl text-start  font-black uppercase ">Frequently asked questions</p>

<div class="border-b-4 w-16 border-primary">&nbsp;</div>



<nav class="accordion w-full   arrows">
<div class="mt-4">

		<input type="radio" name="accordion" id="cb1" />
		<section class="box border-b border-t   border-lightColorBorder hover:border-b-primary shadow-lg hover:shadow-2xl  pr-4 overflow-hidden">
			<label class="box-title  text-xl py-4 text-xl  font-bold text-white w-full flex items-center justify-between h-full" for="cb1"><p>Competition Rules</p>  <i class="fas fa-chevron-down icon-class"></i></label>
			<label class="box-close" for="acc-close"></label>
			<div class="box-content text-base mt-4 text-grayColor pb-4">Your ticket number(s) will be shown as soon as your order is confirmed and will be available under <span class="text-primary font-bold">&lsquo;My Account&rsquo;</span> and in your email confirmation.</div>
		</section>
		<input type="radio" name="accordion" id="cb2" />
		<section class="box border-b border-lightColorBorder hover:border-b-primary shadow-lg hover:shadow-2xl pr-4 overflow-hidden">
      <label class="box-title  py-4 text-xl text-xl font-bold text-white w-full flex items-center justify-between " for="cb2"><p>When is the draw?</p>  <i class="fas fa-chevron-down icon-class"></i></label>
			<label class="box-close" for="acc-close"></label>
			<div class="box-content text-base mt-4 text-grayColor pb-4">Your ticket number(s) will be shown as soon as your order is confirmed and will be available under <span class="text-primary font-bold">&lsquo;My Account&rsquo;</span> and in your email confirmation.</div>
		</section>
		<input type="radio" name="accordion" id="cb3" />
		<section class="box border-b border-lightColorBorder hover:border-b-primary shadow-lg hover:shadow-2xl  pr-4 overflow-hidden">
      <label class="box-title  py-4 text-xl text-xl font-bold text-white w-full flex items-center justify-between " for="cb3"><p>How do you do the draw?</p>  <i class="fas fa-chevron-down icon-class"></i></label>
			<label class="box-close" for="acc-close"></label>
			<div class="box-content text-base mt-4 text-grayColor pb-4">Your ticket number(s) will be shown as soon as your order is confirmed and will be available under <span class="text-primary font-bold">&lsquo;My Account&rsquo;</span> and in your email confirmation.</div>
		</section>
		<input type="radio" name="accordion" id="cb4" />
		<section class="box border-b  border-lightColorBorder hover:border-b-primary shadow-lg hover:shadow-2xl  pr-4 overflow-hidden">
      <label class="box-title  py-4 text-xl text-xl font-bold text-white w-full flex items-center justify-between " for="cb4"><p>What is the draw?</p>  <i class="fas fa-chevron-down icon-class"></i></label>
			<label class="box-close" for="acc-close"></label>
			<div class="box-content text-base mt-4 text-grayColor pb-4">Your ticket number(s) will be shown as soon as your order is confirmed and will be available under <span class="text-primary font-bold">&lsquo;My Account&rsquo;</span> and in your email confirmation.</div>
		</section>
		<input type="radio" name="accordion" id="acc-close" />
    </div>
	</nav>`;

const eventFaqs = `
<nav class="accordion w-full   arrows">
<div class="mt-4"><input id="cb1" name="accordion" type="radio" />
<section class="box border-b border-t   border-lightColorBorder hover:border-b-primary shadow-lg hover:shadow-2xl  pr-4 overflow-hidden">
<p><label class="box-title  text-xl py-4 text-xl w-full flex items-center justify-between  font-bold text-white h-full" for="cb1">Competition Rules<i class="fas fa-chevron-down icon-class">&nbsp;</i></label></p>
<label class="box-close opacity-0" for="acc-close">i</label>

<div class="box-content text-base mt-4 text-grayColor pb-4">Your ticket number(s) will be shown as soon as your order is confirmed and will be available under <span class="text-primary font-bold">&lsquo;My Account&rsquo;</span> and in your email confirmation.</div>
</section>
<input id="cb2" name="accordion" type="radio" />
<section class="box border-b border-lightColorBorder hover:border-b-primary shadow-lg hover:shadow-2xl pr-4 overflow-hidden">
<p><label class="box-title  text-xl py-4 text-xl w-full flex items-center justify-between  font-bold text-white h-full" for="cb2">When is the draw?<i class="fas fa-chevron-down icon-class">&nbsp;</i></label></p>
<label class="box-close opacity-0" for="acc-close">i</label>

<div class="box-content text-base mt-4 text-grayColor pb-4">Your ticket number(s) will be shown as soon as your order is confirmed and will be available under <span class="text-primary font-bold">&lsquo;My Account&rsquo;</span> and in your email confirmation.</div>
</section>
<input id="cb3" name="accordion" type="radio" />
<section class="box border-b border-lightColorBorder hover:border-b-primary shadow-lg hover:shadow-2xl  pr-4 overflow-hidden">
<p><label class="box-title  text-xl py-4 text-xl w-full flex items-center justify-between  font-bold text-white h-full" for="cb3">How do you do the draw?<i class="fas fa-chevron-down icon-class">&nbsp;</i></label></p>
<label class="box-close opacity-0" for="acc-close">i</label>

<div class="box-content text-base mt-4 text-grayColor pb-4">Your ticket number(s) will be shown as soon as your order is confirmed and will be available under <span class="text-primary font-bold">&lsquo;My Account&rsquo;</span> and in your email confirmation.</div>
</section>
<input id="cb4" name="accordion" type="radio" />
<section class="box border-b  border-lightColorBorder hover:border-b-primary shadow-lg hover:shadow-2xl  pr-4 overflow-hidden">
<p><label class="box-title  text-xl py-4 text-xl w-full flex items-center justify-between  font-bold text-white h-full" for="cb4">What is the draw?<i class="fas fa-chevron-down icon-class">&nbsp;</i></label></p>
<label class="box-close opacity-0" for="acc-close">i</label>

<div class="box-content text-base mt-4 text-grayColor pb-4">Your ticket number(s) will be shown as soon as your order is confirmed and will be available under <span class="text-primary font-bold">&lsquo;My Account&rsquo;</span> and in your email confirmation.</div>
</section>
<input id="acc-close" name="accordion" type="radio" /></div>
</nav>

`;

let carouselHeading: any = {};
const findElementsWithAttribute = (node: any, lang: Lang) => {
  if (node.type === 'tag') {
    const shortcode = node?.attribs;
    if (shortcode?.data === 'main-carousel-heading') {
      carouselHeading = JSON.parse(node?.children[0]?.data) as any;
      return <></>;
    }

    if (shortcode?.data === 'main-carousel') {
      const imageCrousel = JSON.parse(node?.children[0]?.data) as [];
      if (imageCrousel.length) {
        return (
          <AboutCarousel
            imageCrousel={imageCrousel}
            heading={carouselHeading}
            lang={lang}
          />
        );
      }
    } else {
      return node;
    }
  }
};

export default function CmsPage({ storeBlogsData }: any) {
  const { lang } = useSelector((state: RootState) => state.layout);
  const { isLogin, user } = useSelector((state: RootState) => state.auth);



  if(storeBlogsData?.slug === 'faq'){
    if(user?.email){
      if ('sendinblue' in window && window?.sendinblue) {
        const sendinblue: any = window.sendinblue;
        sendinblue?.track(
          'faq_page',
          {
            "email": user?.email,
          },
          {
            "data": {}
          },
        ) as any;
      }
    }
  }


  

  const reactElements = parse(
    storeBlogsData?.CMSDescription[lang.lang_id == 1 ? 0 : 1]?.content || '',
    {
      // const reactElements = parse(AboutUsContentTwo || '', {
      replace: (node: any) => findElementsWithAttribute(node, lang),
    },
  );

  if (typeof window !== 'undefined') {
    jqeury('details').click(function (event) {
      jqeury('details').not(this).removeAttr('open');
    });
  }
  return (
    <div className=" w-full bg-bg-1 py-2">
      {storeBlogsData?.slug === 'about-us' || storeBlogsData?.slug === 'faq' ? (
        lang.lang_id == 1 ? (
          <>
            <div className="min-h-screen ">{reactElements}</div>
          </>
        ) : (
          <>
            <div dir={'rtl'} className="min-h-screen">
              {reactElements}
            </div>
          </>
        )
      ) : lang.lang_id == 1 ? (
        <>
          <div
            className="min-h-screen mt-28 cmsStyle px-4 md:px-14 "
            dangerouslySetInnerHTML={
              {
                __html:
                  storeBlogsData?.CMSDescription[0]?.content?.toString() ??
                  'HTML CONTENT NOT FOUND',
              } as any
            }
          />
        </>
      ) : (
        <>
          <div
            dir={'rtl'}
            className="min-h-screen mt-28 cmsStyle px-4 md:px-14"
            dangerouslySetInnerHTML={
              {
                __html:
                  storeBlogsData?.CMSDescription[1]?.content?.toString() ??
                  'لم يتم العثور على المحتوى',
              } as any
            }
          />
        </>
      )}
    </div>
  );
}
