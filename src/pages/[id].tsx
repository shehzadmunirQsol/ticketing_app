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

<div className="w-full relative h-[750px] md:h-[650px] lg:h-[550px] text-center">
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
        <p className="font-black lg:text-5xl md:text-3xl text-xl text-black">Our Mission</p>
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
      <div className=" font-normal max-w-3xl  w-fit">
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
      <p className=" text-white drop-shadow-2xl text-center w-full text-xl  lg:text-3xl">
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
<p classname="font-black lg:text-5xl md:text-3xl text-xl text-black">Our Mission</p>

<p classname="text-background pt-5 text-sm">Winnar, the prestige car raffle company that offers car enthusiasts the thrilling opportunity to win their coveted dream car and many other prizes. Through our captivating experiences and philanthropic initiatives, we aim to create memorable moments that extend beyond the thrill of winning! Join Winnar.com today and get ready to drive away in the car of your dreams.</p>
</div>
</div>

<div classname="absolute top-[610px] right-64 px-8 xl:block hidden"><svg fill="none" height="895" viewbox="0 0 674 1095" width="474" xmlns="http://www.w3.org/2000/svg"> <path d="M1.11379 547.5L486.887 1093.73H819.886L334.113 547.5H1.11379Z" stroke="url(#paint0_linear_190_685)"></path> <path d="M1.11379 546.728L486.887 0.5H819.886L334.113 546.728H1.11379Z" stroke="url(#paint1_linear_190_685)"></path> <defs> <lineargradient gradientunits="userSpaceOnUse" id="paint0_linear_190_685" x1="706" x2="220" y1="1094.23" y2="531.228"> {/* <stop stop-opacity="0"></stop> */} {/* <stop offset="1" stop-color="#454545"></stop> */} </lineargradient> <lineargradient gradientunits="userSpaceOnUse" id="paint1_linear_190_685" x1="706" x2="220" y1="-1.26162e-05" y2="563"> {/* <stop stop-opacity="0"></stop> */} {/* <stop offset="1" stop-color="#454545"></stop> */} </lineargradient> </defs> </svg></div>

<div data="main-carousel-heading">{&quot;headOne&quot;:&quot;MEET OUR DRIVING FORCE&quot;,&quot;pera&quot;:&quot;The Passionate Team Behind Winnar&quot;}</div>
<div data="main-carousel">[ { &quot;img&quot;:&quot;https://media.winnar.com/upload/founder.png&quot;, &quot;heading&quot;:&quot;Scott L. Hughes&quot;, &quot;text&quot;:&quot;Co-Founder&quot;, &quot;hoverhead&quot;:&quot;Scott L. Hughes&quot;, &quot;hoverpera&quot;:&quot;Co-Founder&quot;, &quot;hoverdesc&quot;:&quot;Aenean vulputate eleifend tellus Aenean leo ligula porttitor eu consequat vitae eleifend ac enim. Aliquam lorem ante dapibus in viverra quis feugiat a tellus Phasellus viverra nulla ut metus varius laoreet Quisque rutrum&quot; }, { &quot;img&quot;:&quot;https://media.winnar.com/upload/founder-1.png&quot;, &quot;heading&quot;:&quot;Scott L. Hughes&quot;, &quot;text&quot;:&quot;Founder&quot;, &quot;hoverhead&quot;:&quot;Scott L. Hughes&quot;, &quot;hoverpera&quot;:&quot;Founder&quot;, &quot;hoverdesc&quot;:&quot;Aenean vulputate eleifend tellus Aenean leo ligula porttitor eu consequat vitae eleifend ac enim. Aliquam lorem ante dapibus in viverra quis feugiat a tellus Phasellus viverra nulla ut metus varius laoreet Quisque rutrum&quot; }, { &quot;img&quot;:&quot;https://media.winnar.com/upload/founder-2.png&quot;, &quot;heading&quot;:&quot;Ameen&quot;, &quot;text&quot;:&quot;Founder&quot;, &quot;hoverhead&quot;:&quot;Scott L. Hughes&quot;, &quot;hoverpera&quot;:&quot;Founder&quot;, &quot;hoverdesc&quot;:&quot;Aenean vulputate eleifend tellus Aenean leo ligula porttitor eu consequat vitae eleifend ac enim. Aliquam lorem ante dapibus in viverra quis feugiat a tellus Phasellus viverra nulla ut metus varius laoreet Quisque rutrum&quot; }, { &quot;img&quot;:&quot;https://media.winnar.com/upload/founder-3.png&quot;, &quot;heading&quot;:&quot;Muzi&quot;, &quot;text&quot;:&quot;Founder&quot;, &quot;hoverhead&quot;:&quot;Scott L. Hughes&quot;, &quot;hoverpera&quot;:&quot;Founder&quot;, &quot;hoverdesc&quot;:&quot;hello&quot; }, { &quot;img&quot;:&quot;https://media.winnar.com/upload/founder.png&quot;, &quot;heading&quot;:&quot;Ahmed&quot;, &quot;text&quot;:&quot;Founder&quot;, &quot;hoverhead&quot;:&quot;Scott L. Hughes&quot;, &quot;hoverpera&quot;:&quot;Founder&quot;, &quot;hoverdesc&quot;:&quot;hello&quot; }, { &quot;img&quot;:&quot;https://media.winnar.com/upload/founder.png&quot;, &quot;heading&quot;:&quot;Shehzad&quot;, &quot;text&quot;:&quot;Founder&quot;, &quot;hoverhead&quot;:&quot;Scott L. Hughes&quot;, &quot;hoverpera&quot;:&quot;Founder&quot;, &quot;hoverdesc&quot;:&quot;Aenean vulputate eleifend tellus Aenean leo ligula porttitor eu consequat vitae eleifend ac enim. Aliquam lorem ante dapibus in viverra quis feugiat a tellus Phasellus viverra nulla ut metus varius laoreet Quisque rutrum&quot; } ]</div>


<div classname="w-full py-32 lg:px-14 md:px-14 px-4 z-40 text-center bg-Image">
<div classname=" text-center flex flex-col lg:flex-row gap-x-4 justify-center items-start font-sans lg:mx-auto">
<p classname=" text-white drop-shadow-2xl text-center  font-black text-2xl  lg:text-3xl lg:w-fit">OUR MVP</p>

<div classname=" font-normal max-w-3xl w-fit">
<p classname=" text-white drop-shadow-2xl text-justify text-sm ">At Winnar, we offer discerning individuals an exclusive opportunity to win their coveted dream car from our meticulously curated collection of performance, luxury, and prestige brands.</p>

<p classname=" text-white drop-shadow-2xl text-sm text-justify  pt-10">What sets us apart is our unwavering focus on transparency, fairness, and excitement. Unlike traditional car purchasing methods, Winnar.com provides an electrifying platform where every participant is empowered to drive away in the car of their dreams. With our transparent selection process, participants can be confident that their chances are fair, and the thrill of winning is unparalleled. Join us today to experience the epitome of automotive aspirations and be a part of an exceptional raffle journey.</p>
</div>
</div>
</div>

<div classname="w-full relative h-[250px] lg:h-[550px] z-40 text-center"><img alt="/" classname=" object-cover w-full  h-full block bg-black/50" fill="" quality="{100}" src="https://media.winnar.com/upload/about-background-2.png" />
<div classname="absolute h-[35px] w-full text-center top-[50%] flex items-center">
<div classname="w-full text-center ">
<p classname=" text-white drop-shadow-2xl text-center w-full text-xl  lg:text-3xl">Get your dream car at a fraction of the price!</p>

<p classname="relative h-[35px] -top-[50%]   mt-6 text-white drop-shadow-2xl text-center w-full text-2xl  lg:text-4xl font-[900]">WIN SUNDAY 8 PM</p>
<button classname=" px-8 mt-6 py-2  bg-primary clip-style text-black font-sans font-[900] text-xl" variant="default">Enter Now</button></div>
</div>
</div>
</div>

`;

const faqUpdateArea = `
<div class="w-full relative h-[750px] md:h-[650px] lg:h-[550px] z-40 text-center "><img alt="/" class="object-cover  w-full h-full block bg-black/50" fill="" quality="{100}" src="https://media.winnar.com/upload/faq-banner.png" />
<div class="absolute h-[35px] w-full text-center top-[50%] flex items-center">
<div class="w-full text-center">
<p class="text-white drop-shadow-2xl text-center w-full text-4xl  lg:text-5xl tracking-tighter   font-[900]">FAQs</p>
</div>
</div>
</div>

<div classname="absolute top-[40px] right-64 px-8 xl:block hidden"><svg fill="none" height="895" viewbox="0 0 674 1095" width="474" xmlns="http://www.w3.org/2000/svg"> <path d="M1.11379 547.5L486.887 1093.73H819.886L334.113 547.5H1.11379Z" stroke="url(#paint0_linear_190_685)"></path> <path d="M1.11379 546.728L486.887 0.5H819.886L334.113 546.728H1.11379Z" stroke="url(#paint1_linear_190_685)"></path> <defs> <lineargradient gradientunits="userSpaceOnUse" id="paint0_linear_190_685" x1="706" x2="220" y1="1094.23" y2="531.228"> {/* <stop stop-opacity="0"></stop> */} {/* <stop offset="1" stop-color="#454545"></stop> */} </lineargradient> <lineargradient gradientunits="userSpaceOnUse" id="paint1_linear_190_685" x1="706" x2="220" y1="-1.26162e-05" y2="563"> {/* <stop stop-opacity="0"></stop> */} {/* <stop offset="1" stop-color="#454545"></stop> */} </lineargradient> </defs> </svg></div>

<div class="relative w-full flex flex-col gap-8  lg:px-14 md:px-14 px-4 mt-16 mb-24">
<div>
<p class="lg:text-5xl md:text-4xl text-3xl text-start  font-black uppercase ">Frequently asked questions</p>

<div class="border-b-4 w-16 border-primary">&nbsp;</div>



<nav class="accordion w-full   arrows">
<div class="mt-4">

		<input type="radio" name="accordion" id="cb1" />
		<section class="box border-b border-t   border-lightColorBorder hover:border-b-primary shadow-lg hover:shadow-2xl  pr-4 overflow-hidden">
			<label class="box-title  lg:text-3xl py-4 text-xl  font-bold text-white w-full flex items-center justify-between h-full" for="cb1"><p>Competition Rules</p>  <i class="fas fa-chevron-down icon-class"></i></label>
			<label class="box-close" for="acc-close"></label>
			<div class="box-content text-base mt-4 text-grayColor pb-4">Your ticket number(s) will be shown as soon as your order is confirmed and will be available under <span class="text-primary font-bold">&lsquo;My Account&rsquo;</span> and in your email confirmation.</div>
		</section>
		<input type="radio" name="accordion" id="cb2" />
		<section class="box border-b border-lightColorBorder hover:border-b-primary shadow-lg hover:shadow-2xl pr-4 overflow-hidden">
      <label class="box-title  py-4 lg:text-3xl text-xl font-bold text-white w-full flex items-center justify-between " for="cb2"><p>When is the draw?</p>  <i class="fas fa-chevron-down icon-class"></i></label>
			<label class="box-close" for="acc-close"></label>
			<div class="box-content text-base mt-4 text-grayColor pb-4">Your ticket number(s) will be shown as soon as your order is confirmed and will be available under <span class="text-primary font-bold">&lsquo;My Account&rsquo;</span> and in your email confirmation.</div>
		</section>
		<input type="radio" name="accordion" id="cb3" />
		<section class="box border-b border-lightColorBorder hover:border-b-primary shadow-lg hover:shadow-2xl  pr-4 overflow-hidden">
      <label class="box-title  py-4 lg:text-3xl text-xl font-bold text-white w-full flex items-center justify-between " for="cb3"><p>How do you do the draw?</p>  <i class="fas fa-chevron-down icon-class"></i></label>
			<label class="box-close" for="acc-close"></label>
			<div class="box-content text-base mt-4 text-grayColor pb-4">Your ticket number(s) will be shown as soon as your order is confirmed and will be available under <span class="text-primary font-bold">&lsquo;My Account&rsquo;</span> and in your email confirmation.</div>
		</section>
		<input type="radio" name="accordion" id="cb4" />
		<section class="box border-b  border-lightColorBorder hover:border-b-primary shadow-lg hover:shadow-2xl  pr-4 overflow-hidden">
      <label class="box-title  py-4 lg:text-3xl text-xl font-bold text-white w-full flex items-center justify-between " for="cb4"><p>What is the draw?</p>  <i class="fas fa-chevron-down icon-class"></i></label>
			<label class="box-close" for="acc-close"></label>
			<div class="box-content text-base mt-4 text-grayColor pb-4">Your ticket number(s) will be shown as soon as your order is confirmed and will be available under <span class="text-primary font-bold">&lsquo;My Account&rsquo;</span> and in your email confirmation.</div>
		</section>
		<input type="radio" name="accordion" id="acc-close" />
    </div>
	</nav>`;

const termsCondition = `
<div class="mt-24 px-4 lg:px-14 md:px-14 ">
<p class="lg:text-4xl md:text-3xl text-2xl text-start  font-black uppercase ">أحكام وشروط</p>

<div class="border-b-4 w-16 border-primary ">&nbsp;</div>

<div class="mt-10 space-y-10">
<p class="lg:text-4xl md:text-3xl text-2xl mb-4">الشروط والأحكام الخاصة بشركة WINNAR TRADING LLC</p>

<p >تشكل هذه الشروط والأحكام اتفاقية ملزمة قانونًا مبرمة بينك، سواء شخصيًا أو نيابة عن كيان ("أنت") وشركة WINNAR TRADING LLC ("نحن" أو "نا" أو "خاصتنا")، فيما يتعلق بوصولك إلى واستخدامك لموقع www.winnar.com بالإضافة إلى أي نموذج وسائط آخر أو قناة إعلامية أو هاتف محمول أو موقع ويب أو تطبيق للهاتف المحمول مرتبط به أو مرتبط به أو متصل به بطريقة أخرى (يُشار إليه إجمالاً بـ "موقع الويب").</p>

<p >أنت توافق على أنه من خلال دخولك إلى الموقع، فإنك قد قرأت وفهمت ووافقت على الالتزام بجميع هذه الشروط والأحكام. إذا كنت لا توافق على جميع هذه الشروط والأحكام، يُحظر عليك صراحةً استخدام الموقع ويجب عليك التوقف عن استخدامه على الفور.
</p>

<p>يتم تضمين الشروط والأحكام التكميلية أو المستندات التي قد يتم نشرها على الموقع من وقت لآخر صراحةً هنا بالإشارة إليها بما في ذلك على سبيل المثال لا الحصر، سياسة الخصوصية وسياسة ملفات تعريف الارتباط. نحن نحتفظ بالحق، وفقًا لتقديرنا الخاص، في إجراء تغييرات أو تعديلات على هذه الشروط والأحكام في أي وقت ولأي سبب كان..</p>

<p >تقع على عاتقك مسؤولية مراجعة هذه الشروط والأحكام بشكل دوري للبقاء على اطلاع بالتحديثات. ستخضع وستعتبر على علم بالتغييرات في أي شروط وأحكام منقحة وقبلتها من خلال استمرارك في استخدام الموقع بعد تاريخ نشر هذه الشروط والأحكام المنقحة.</p>


<ol class="decimal space-y-8 px-4 ">
<li ><strong>حقوق الملكية الفكرية</strong>
<ol class="decimal space-y-8 ltr:ml-8 rtl:mr-8">
<li>ما لم تتم الإشارة إلى خلاف ذلك، فإن موقع الويب هو ملكية خاصة بنا وجميع أكواد المصدر وقواعد البيانات والوظائف والبرامج وتصميمات موقع الويب والصوت والفيديو والنصوص والصور الفوتوغرافية والرسومات الموجودة على موقع الويب (يُشار إليها إجمالاً باسم "المحتوى") والعلامات التجارية والخدمات العلامات والشعارات الواردة فيها ("العلامات") مملوكة لنا أو خاضعة لسيطرتنا أو مرخصة لنا، وهي محمية بموجب قوانين حقوق الطبع والنشر والعلامات التجارية ومختلف حقوق الملكية الفكرية الأخرى لدولة الإمارات العربية المتحدة والاتفاقيات الدولية.
</li>
<li>"الملكية الفكرية" تعني جميع العلامات التجارية وبراءات الاختراع والاختراعات (سواء كانت قابلة للحصول على براءة اختراع أم لا) وحقوق التأليف والنشر والحقوق الأخلاقية وتصميمات المنتجات وحقوق التصميم والأسماء التجارية والأسماء التجارية وعلامات الخدمة والشعارات وأسماء الخدمات والأسرار التجارية وأسماء النطاقات وحقوق قاعدة البيانات. وأي ملكية فكرية أو حقوق ملكية أخرى (سواء كانت مسجلة أو غير مسجلة) بما في ذلك الحقوق في برامج الكمبيوتر وجميع التسجيلات والتطبيقات لتسجيل أي من العناصر المذكورة أعلاه.
</li>
</ol>
</li>

<li ><strong>استخدام محتوى الموقع</strong>

<ol class="decimal space-y-8 ltr:ml-8 rtl:mr-8">
<li>جميع المعلومات والنصوص والمواد والرسومات والمنتجات وأسماء المنتجات والصور والتصاميم وخطوط العلامات والشعارات وأيقونات الأزرار والصور ومقاطع الفيديو والصوت والعلامات التجارية والأسماء التجارية وأسماء الخدمات (سواء كانت مسجلة أم لا) والإعلانات والتخطيط يتم توفير الترتيب وواجهة المستخدم الرسومية والشكل والمظهر وميزات التحكم الخاصة بالموقع لك كخدمة للسماح لك وللمستخدمين الآخرين بتصفح منتجاتنا وخدماتنا.
</li>
<li>يُسمح لك فقط بالوصول إلى هذا الموقع واستخدامه لأغراضك الشخصية وغير التجارية، ولا يجوز لك القيام بما يلي:</li>

<ol class="alpha space-y-2 ltr:ml-8 rtl:mr-8  ltr:sm:ml-12 rtl:sm:mr-12">
<li>تعديل أو نشر أو نقل أو نقل أو بيع أو إعادة إنتاج أو إنشاء أعمال مشتقة من أو توزيع أو تنفيذ أو عرض أو استغلال أي محتوى من محتوى موقع الويب بأي شكل من الأشكال، كليًا أو جزئيًا، باستثناء ما هو مسموح به صراحةً في هذه الشروط؛
</li>
<li>استخدام هذا الموقع في انتهاك لأي قانون أو لائحة معمول بها؛</li>
<li>استخدام هذا الموقع لنقل أو توزيع أو تخزين أو تدمير المواد، بما في ذلك على سبيل المثال لا الحصر محتوى الموقع، بطريقة تنتهك حقوق الطبع والنشر أو العلامات التجارية أو الأسرار التجارية أو غيرها من حقوق الملكية الفكرية أو غيرها أو تنتهك الخصوصية أو الدعاية أو الحقوق الشخصية الأخرى الخاصة بنا. آحرون؛
</li>
<li>بيع أو تعديل أو حذف محتوى موقع الويب أو إعادة إنتاج أو عرض أو تنفيذ أو توزيع أو استخدام محتوى موقع الويب بأي شكل من الأشكال لأي غرض عام أو تجاري؛
</li>
<li>الوصول إلى البيانات غير المخصصة لك أو تسجيل الدخول إلى خادم أو حساب غير مصرح لك بالوصول إليه؛</li>
<li>محاولة فحص أو فحص أو اختبار مدى ضعف نظام أو شبكة أو خرق إجراءات الأمان أو المصادقة دون الحصول على إذن مناسب؛</li>
<li> نسخ أو تعديل كود HTML الذي تم إنشاؤه لإنشاء أي محتوى موقع ويب أو الصفحات التي يتكون منها موقع الويب؛
</li>
<li> استخدام محتوى موقع الويب لانتحال شخصية أو تحريف هويتك أو انتمائك إلينا أو إلى أي شخص أو كيان آخر؛
</li>
<li>استخدام محتوى موقع الويب لإرسال بريد أو بريد إلكتروني غير مرغوب فيه، أو إجراء مكالمات هاتفية غير مرغوب فيها أو إرسال رسائل فاكس غير مرغوب فيها بخصوص العروض الترويجية و/أو الإعلان عن المنتجات أو الخدمات؛
</li>
<li>استخدام أي جهاز أو برنامج أو روتين للتدخل أو محاولة التدخل في العمل السليم للموقع أو أي نشاط يتم إجراؤه على الموقع، بما في ذلك على سبيل المثال لا الحصر، المواد التي تحتوي على فيروسات أو أحصنة طروادة أو الفيروسات المتنقلة أو القنابل الموقوتة أو برامج الإلغاء أو أجهزة الكمبيوتر الأخرى إجراءات البرمجة أو المحركات التي تهدف إلى إلحاق الضرر بالتدخل الضار في أي نظام أو بيانات أو معلومات أو اعتراضها خلسة أو مصادرتها؛
</li>
<li>محاولة فك تشفير أو تفكيك أو تفكيك أو إجراء هندسة عكسية لأي من البرامج التي تشتمل على الموقع الإلكتروني أو تشكل جزءًا منه بأي شكل من الأشكال؛ أو</li>
<li>إطار أو رابط لأي محتوى موقع ويب أو معلومات متاحة من الموقع.</li>
</ol>

<li>لا يجوز لك نسخ أو تخزين أو إعادة توزيع أو نشر أي محتوى موقع دون الحصول على إذن كتابي منا.</li>
</ol>
</li>



<li ><strong>تواصل</strong>
<ol class="decimal space-y-8 ltr:ml-8 rtl:mr-8">
<li>من أجل الامتثال لالتزاماتنا بموجب هذه الشروط والأحكام، وأي سياسة و/أو إعلان آخر منشور عبر موقعنا الإلكتروني، من وقت لآخر، قد نتواصل معك عبر البريد الإلكتروني، أو عن طريق أشكال أخرى من الرسائل الإلكترونية و/أو عن طريق إرسال إشعارات إلى حسابك المسجل لدينا.
</li>
<li>أنت توافق وتقر بموجب هذا بأنه يجوز لك تلقي مراسلات وإخطارات منا إلكترونيًا وأن هذه المراسلات والإخطارات ستلبي أي متطلبات قانونية للمراسلات التي يلزم أن تكون كتابية.
</li>
</ol>
</li>


<li ><strong>طريقة الدخول في السحب</strong>
<ol class="decimal space-y-8 ltr:ml-8 rtl:mr-8">
<li>>نبيع زجاجات المياه، والتي يمكن شراؤها من خلال موقعنا الإلكتروني أو أي طريقة أخرى نقدمها من وقت لآخر ("المنتجات"). ليس لمنتجاتنا سعر ثابت ونحتفظ بالحق في تحديد سعر منتجاتنا، بناءً على السحب المحدد الذي ترغب في المشاركة فيه. باستخدام خدماتنا، فإنك توافق على أنه ليس لديك أي مطالبات فيما يتعلق بقرارات التسعير لدينا، مهما كانت. وتوافق أيضًا على الالتزام بشروط وأحكام كل مسابقة سحب ترغب في المشاركة فيها.
<p class="mt-4"><strong>سيتم الإشارة إلى أسعار منتجاتنا بوضوح على موقعنا. أنت تقر وتوافق وتؤكد أنه تقع على عاتقك وحدك مسؤولية مراجعة وتأكيد أسعار منتجاتنا قبل الشروع في أي عملية شراء. من خلال إكمال عملية شراء منتجاتنا، فإنك تؤكد إقرارك بالأسعار التي نقدمها لمنتجاتنا، والتي قد يتم تحديثها و/أو تعديلها من وقت لآخر.
</strong></p>
</li>
<li>أنت تقر بأنه سيتم التبرع بالمنتج تلقائيًا إلى المحتاجين من خلال شركائنا في المجتمع أو المؤسسات الخيرية المسجلة. من خلال شراء منتج والتبرع به، سوف تتلقى تلقائيًا تذكرة دخول واحدة (x1) إلى السحب القادم.
</li>
</li>
<li>لا يوجد أي قيود على الحد الأقصى لعدد عمليات شراء المنتج و/أو عدد تذاكر الدخول التي يمكنك الحصول عليها، حسب التوفر. ومع ذلك، قد يفرض شركاء معالجة الدفع لدينا حدودًا على عدد المعاملات و/أو المدفوعات المسموح لك بإجرائها ولا نتحمل أي مسؤولية على الإطلاق، في حالة رفض المعاملة و/أو الدفع و/أو رفضها من خلال معالجة الدفع لدينا. الشركاء. ننصحك بمراجعة الشروط والسياسات الخاصة بشركائنا في معالجة الدفع لفهم الحدود والشروط المحددة التي قد تنطبق على معاملاتك و/أو مدفوعاتك.
</li>
<li>من خلال شراء المنتج، فإنك تقر وتفهم أنك لن تتلقى المنتج ويجب ألا تتوقع استلام المنتج منا أو من أي طرف ثالث شريك، وأنه سيتم التبرع بالمنتج (المنتجات) تلقائيًا إلى أي مؤسسة خيرية. نحن نقرر من وقت لآخر، من خلال شركائنا في المجتمع.
</li>
<li>من خلال شراء المنتج، فإنك تؤكد أنك قد قرأت وفهمت هذه الشروط والأحكام. أنت تدرك تمامًا وتقر بالشرط المنصوص عليه في هذه الشروط والأحكام فيما يتعلق بالتبرع التلقائي بالمنتج (المنتجات) لأي مؤسسة خيرية نقررها من وقت لآخر، من خلال شركائنا في المجتمع. تشير عملية الشراء الخاصة بك إلى مساهمتك التطوعية في مساعينا الخيرية، ونحن نقدر دعمك في إحداث تأثير إيجابي على المحتاجين.
</li>
</ol>
</li>


<li ><strong>تمديد السحب</strong>
<ol class="decimal space-y-8 ltr:ml-8 rtl:mr-8">
<li>
نحن نحتفظ بالحق في تمديد السحب في حالة عدم بيع التذاكر المتاحة، في وقت إغلاق مسابقة السحب، مما يضمن سحبًا عادلاً وتنافسيًا لجميع المشاركين؛
</li>
<li>
أنت تقر وتوافق على أنه يجوز لنا تمديد سحب السحب قبل انتهاء الوقت المحدد لكل مسابقة سحب؛ و
</li>
<li>
نحن نحتفظ بالحق في تمديد مسابقة السحب وفقًا لتقديرنا الخاص، عندما نرى ذلك ضروريًا للأداء المناسب لواجباتنا تجاهك وتجاه جميع المشاركين.
</li>

<li>
متى حدث هذا التمديد، فسنخطرك على الفور من خلال وسائل مختلفة، بما في ذلك على سبيل المثال لا الحصر، البريد الإلكتروني أو إشعار الحساب أو إشعار التطبيق أو أي وسيلة اتصال معقولة أخرى متاحة لنا في ذلك الوقت.
</li>

<li>
تقع على عاتقك وحدك مسؤولية البقاء على اطلاع من خلال المتابعة النشطة لأي رسائل بريد إلكتروني وإشعارات (من أي نوع) نقدمها لك ويجب أن تظل على اطلاع دائم بمسابقة السحب التي شاركت فيها.
</li>

</ol>
</li>



<li ><strong>جائزة</strong>
<ol class="decimal space-y-8 ltr:ml-8 rtl:mr-8">
<li>
قد تقدم كل مسابقة سحب جائزة واحدة أو أكثر، والتي يتم وصفها في صفحة المسابقة ذات الصلة على موقعنا الإلكتروني ("الجائزة"). التفاصيل المقدمة على الموقع الإلكتروني، فيما يتعلق بالجائزة، هي، على حد علمنا ومعلوماتنا واعتقادنا، دقيقة اعتبارًا من تاريخ افتتاح مسابقة السحب.
</li>
<li>
كل مسابقة سحب مرخصة من قبل الهيئة التنظيمية المختصة وتمتلك رقم تصريح صالح، والذي سيتم الإشارة إليه في صفحة المسابقة على موقعنا. تلتزم جميع السحوبات بأنظمة وقوانين دولة الإمارات العربية المتحدة، بصيغتها المعدلة من وقت لآخر.
</li>
<li>
المعلومات المقدمة على الموقع، بما في ذلك تفاصيل حول الجائزة وقيمتها وحالتها والمعلومات الأخرى ذات الصلة، يتم توفيرها "كما هي" دون أي إقرارات أو ضمانات، صريحة أو ضمنية. نحن لا نضمن دقة أو اكتمال أو توقيت المعلومات المقدمة على الموقع.
</li>

<li>
الجوائز غير النقدية تخضع لمدى توفرها. في حالة عدم توفر الجائزة الأصلية، فإننا نحتفظ بموجب هذا بالحق في استبدالها بجائزة ذات قيمة مساوية أو أكبر، مما يضمن حصولك على تجربة مجزية. في حالة حدوث أي تغييرات في تفاصيل الجائزة، سنبذل قصارى جهدنا لتحديث الموقع الإلكتروني على الفور، مما يضمن إبلاغك على الفور وبقائك على اطلاع بأحدث المعلومات.
</li>

<li>
نحتفظ بالحق في استبدال الجائزة بجائزة نقدية بديلة في الحالات التالية:
<ol class="alpha space-y-2  ltr:ml-8 rtl:mr-8  ltr:sm:ml-12 rtl:sm:mr-12  ">
<li>تصبح الجائزة غير متاحة</li>
<li>ظروف أخرى خارجة عن سيطرتنا المعقولة
</li>
</ol>
</li>

<li>
نحن لا نتحمل أي مسؤولية عن أي تناقضات أو أخطاء في تفاصيل الجوائز ومدى توفرها. على الرغم من أننا نبذل قصارى جهدنا لضمان دقة المحتوى الموجود على الموقع، إلا أننا لا نضمن أن تكون المعلومات المقدمة فيه محدثة دائمًا و/أو خالية من الأخطاء.
</li>

<li>
باستخدام الموقع والمشاركة في مسابقة السحب، فإنك توافق وتقر وتقبل المخاطر الكامنة المرتبطة بالاعتماد على محتوى الموقع، وتحررنا من أي مسؤولية من هذا القبيل في هذا الصدد.
</li>

<li>
أنت تقر وتوافق على أن الفائز في مسابقة السحب هو فرد يمتثل لمتطلبات البند 9.7 ويتم اختياره من خلال سحب عشوائي، لتلقي الجائزة المخصصة التي نقدمها بموجب البند 6 ("الفائز").
</li>
<li>
يتم اختيار الفائز من بين مجموعة من المشاركين الذين حصلوا على تذكرة (تذاكر) السحب وفقًا للبند 4 وقواعد المنافسة على النحو المنصوص عليه في هذه الوثيقة. أنت تقر وتوافق على أن الفوز في مسابقة السحب يعتمد فقط على الصدفة، ولا يتم منح أي ضمانات من أي نوع لأي مشارك. يتم تحديد نتيجة مسابقة السحب من خلال عملية اختيار عشوائية، وتكون النتائج نهائية. كمشارك، فإنك تدرك أن طبيعة المسابقة لا تضمن أي نتيجة محددة، وأن توزيع الجوائز يعتمد بالكامل على الحظ والصدفة.
</li>

<li>
أنت تقر وتوافق على أنه تم اختيار الفائز بالكامل عن طريق الصدفة وخارج نطاق تأثيرنا. لا يمكننا التدخل بأي شكل من الأشكال للتأثير على النتائج أو تعيين الفائز بأي شكل من الأشكال.
</li>
<li>
إذا كانت الجائزة مركبة:
<ol class="decimal space-y-2 ltr:sm:ml-12 rtl:sm:mr-12">
<li>لا تشمل الجائزة أي تأمين، ويتحمل الفائز المسؤولية الكاملة عن الحصول على التغطية التأمينية الكافية للمركبة قبل تشغيلها على الطرق العامة؛
</li>
<li>يتحمل الفائز وحده المسؤولية عن تغطية أي تكاليف مرتبطة باختبار إدارة المرور والتسجيل المطلوب لاستخدام السيارة على الطرق العامة. الفائز مسؤول أيضًا عن الالتزام بجميع المتطلبات المنصوص عليها من قبل السلطات ذات الصلة للتسجيل السليم والامتثال لأي وجميع القوانين واللوائح والمتطلبات المحلية. تقع على عاتق الفائز وحده مسؤولية التأكد من أن أي تعديلات أو إصلاحات أو خدمات ضرورية للوفاء بهذا الامتثال تكون على نفقة الفائز..
</li>
<li>بمجرد استلام الجائزة من قبل الفائز، لا نتحمل أي مسؤولية أخرى عنها. يتحمل الفائز وحده المسؤولية عن الامتثال لجميع القوانين واللوائح ذات الصلة المتعلقة بالسيارة، بما في ذلك على سبيل المثال لا الحصر، تشغيلها وصيانتها وضمان استخدامها بطريقة آمنة ومسؤولة؛ و
</li>

<li>الفائز هو المسؤول عن الحصول على واستخدام جميع معدات وملابس السلامة اللازمة (حيثما ينطبق ذلك) أثناء تشغيل السيارة.
</li>

<li>الضمان تنويه
<ol class="alpha space-y-2   ltr:ml-8 rtl:mr-8  ltr:sm:ml-12 rtl:sm:mr-12   ml-10 sm:ml-16">
<li>يتم منح السيارة المقدمة كجائزة في كل مسابقة سحب "كما هي" و"أينما كانت" دون أي ضمانات صريحة أو ضمنية من أي نوع.
</li>
<li>نحن لا نقدم أي تعهدات و/أو ضمانات فيما يتعلق بالحالة أو الجودة أو الأداء أو الملاءمة لغرض معين لجائزة السيارة.
<li>لا تأتي الجائزة عندما تكون السيارة مصحوبة بأي ضمان من الشركة المصنعة، ونحن لا نضمن توفر أي ضمان من هذا القبيل على الإطلاق.
</li>
<li>إلى أقصى حد يسمح به القانون (القوانين) واللوائح المعمول بها، نحن نخلي مسؤوليتنا من جميع الضمانات، سواء كانت قانونية أو صريحة أو ضمنية، بما في ذلك على سبيل المثال لا الحصر ضمانات القابلية للتسويق والملاءمة لغرض معين وعدم الانتهاك.
</li>
<li>نحن لا نتحمل أي مسؤولية على الإطلاق عن أي أضرار أو تكاليف أو مصاريف أو خسائر أو التزامات ناشئة عن الجائزة أو مرتبطة بها عندما تكون السيارة، بما في ذلك على سبيل المثال لا الحصر، أي مشكلات أو عيوب أو قصور في حالة السيارة أو أدائها أو وظائفها.
</li>
<li>يعد إخلاء المسؤولية عن الضمان جزءًا لا يتجزأ من هذه الشروط والأحكام وينطبق إلى الحد الأقصى الذي تسمح به القوانين المعمول بها.
</li>
</ol>
</li>


</ol>
</li>


<li>قد تخضع بعض الجوائز غير النقدية لشروط إضافية، والتي سيتم تحديدها بوضوح في صفحة المسابقة ذات الصلة بالموقع في وقت المشاركة. من الضروري أن يقوم المشاركون بمراجعة هذه الشروط الإضافية والالتزام بها، لأنها قد تؤثر على الأهلية أو عملية الاسترداد أو أي متطلبات محددة أخرى تتعلق بالجائزة. من خلال المشاركة في المسابقة، فإنك توافق على الالتزام بأي شروط إضافية مرتبطة بالجوائز غير النقدية.
</li>

<li>يجوز تقديم الجائزة من قبل طرف ثالث، وإذا كان ذلك ممكنًا، سيتم توفير التفاصيل ذات الصلة بمورد الطرف الثالث على صفحة المنافسة على الموقع الإلكتروني. من المهم ملاحظة أن الشروط والأحكام الخاصة بالمورد الخارجي قد تنطبق أيضًا على الجائزة، ويتم تشجيع المشاركين على مراجعة أي شروط أو متطلبات إضافية يحددها المورد والامتثال لها. من خلال المشاركة في المسابقة وقبول الجائزة، فإنك توافق على الالتزام بأي من هذه الشروط والأحكام الخاصة بالمورد الخارجي، إن أمكن. ومع ذلك، فإننا لا نتحمل أي مسؤولية على الإطلاق، فيما يتعلق بشروط وأحكام الموردين الخارجيين أو إقراراتهم أو ضماناتهم.
</li>
<li>الجائزة غير قابلة للتفاوض أو التحويل إلى أي فرد آخر غير الفائز المعلن. تُمنح الجائزة للفائز حصريًا ولا يمكننا التنازل عنها أو بيعها أو استبدالها بأي شخص أو كيان آخر.
</li>
<li>الفائز هو المسؤول الوحيد عن جميع التكاليف والنفقات التي لم يتم تغطيتها صراحةً في تفاصيل الجائزة. أي نفقات أو رسوم إضافية يتم تكبدها خارج نطاق الجائزة المحددة هي مسؤولية الفائز وحده.
</li>
</ol>
</li>



<li ><strong>بديل الجائزة النقدية</strong>
<ol class="decimal space-y-8 ml-8">
<li>إذا حصل الفائز في مسابقة السحب على سيارة كجائزة، فإننا نقدم خيار البديل النقدي، المعروف باسم "بديل الجائزة النقدية". خلال الإطار الزمني المحدد المنصوص عليه في البند 16.2، قبل المطالبة بجائزة السيارة، يكون للفائز خيار الاتصال بنا والتعبير عن نيته الحصول على جائزة نقدية بدلاً من السيارة.
</li>
<li>سيتم ذكر إمكانية بديل الجائزة النقدية والقيمة المقابلة لها بشكل صريح في المواد الإعلانية والترويجية الخاصة بمسابقة السحب ذات الصلة. تقع على عاتقك مسؤولية قراءة جميع التفاصيل الواردة في هذه المواد الإعلانية والترويجية بدقة قبل الدخول في مسابقة السحب.
</li>
<li>سيخضع بديل الجائزة النقدية، إذا اختاره الفائز، لجميع الضرائب والرسوم المطبقة والتي تتطلبها القوانين واللوائح ذات الصلة. يجب أن يلتزم الفائز بأية التزامات ولوائح ضريبية مرتبطة بتلقي بديل الجائزة النقدية، حسب الاقتضاء في ولايته القضائية.
</li>
</ol>
</li>




<li ><strong>نتائج السحب</strong>
<ol class="decimal space-y-8 ml-8">
<li>سيتم اختيار الفائز من قبل ممثل رسمي من الجهة الحكومية ذات الصلة.
</li>
<li>سيتم إخطار الفائز في نفس يوم إجراء السحب.</li>
<li>سيتم الإعلان عن اسم الفائز علنًا في الصحافة المحلية والإقليمية ومنصات التواصل الاجتماعي المرتبطة بنا رسميًا، ما لم تقدم لنا طلبًا كتابيًا بالامتناع عن مثل هذه الإعلانات قبل تاريخ السحب.
</li>
<li>سيتم أيضًا إخطار الفائز عبر عنوان البريد الإلكتروني المقدم أثناء عملية تسجيل الحساب.
</li>
<li>ما لم يُنص صراحة على خلاف ذلك في صفحة المسابقة على موقعنا الإلكتروني، يجب أن يكون عدد الفائزين في كل سحب دائمًا واحدًا (1).
</li>
</ol>
</li>




<li ><strong>الأهلية للشراء</strong>
<ol class="decimal space-y-8 ml-8">
<li>يقتصر شراء منتجاتنا من خلال الموقع الإلكتروني بشكل صارم على الأطراف الذين يمكنهم بشكل قانوني إبرام العقود وإبرامها على الإنترنت، وفقًا لقوانين دولة الإمارات العربية المتحدة. تقع على عاتقك مسؤولية مطلقة التأكد من استيفاء المتطلبات القانونية للمشاركة في مثل هذه المعاملات ضمن الولاية القضائية المعمول بها.
</li>
<li>من أجل إجراء عمليات شراء على الموقع، سيُطلب منك تقديم بياناتك الشخصية أثناء عملية تسجيل الحساب، بما في ذلك (1) اسمك الحقيقي الكامل، (2) رقم الهاتف، (3) عنوان البريد الإلكتروني، (4) الموقع، ( 5) عنوان السكن، وأية معلومات أخرى مطلوبة. لكي تعتبر عميلاً لدينا، لا يجب عليك تسجيل حسابك فحسب، بل يجب عليك أيضًا إكمال عملية التحقق ومن ثم تنشيط حسابك. بالإضافة إلى ذلك، يجب عليك تقديم تفاصيل دفع دقيقة وصالحة، والتأكيد على أن معلومات الفواتير المقدمة تخصك وأنها صحيحة. من خلال تقديم هذه المعلومات، فإنك تقر وتضمن أن التفاصيل دقيقة وأنك المالك المعتمد لطريقة الدفع المستخدمة في المعاملات.
</li>
</ol>
</li>



<li><strong>شراء المنتجات</strong>
    <ol class="decimal space-y-8 ml-8">
    <li>منتجاتنا متاحة حصريًا للشراء من خلال موقعنا الآمن. في الوقت الحالي، نقوم بتسهيل المعاملات فقط باستخدام بطاقة الخصم أو بطاقة الائتمان وApplePay وGoogle Pay والعملات المشفرة عبر موفري خدمات خارجيين. ومع ذلك، فإننا ندرك أهمية توفير خيارات دفع متنوعة لاستيعاب تفضيلات عملائنا، وبالتالي، يجوز لنا، وفقًا لتقديرنا المطلق، تقديم طرق دفع إضافية في المستقبل.

    </li>
    <li>أنت تقر وتوافق على أنه في بعض الأحيان، قد لا تتم معالجة المدفوعات أو قد يتم رفضها لأسباب مختلفة، بما في ذلك على سبيل المثال لا الحصر، المشكلات الفنية أو عدم كفاية الأموال أو المخاوف الأمنية. نحن لا نتحمل أي مسؤولية في هذا الصدد وأنت تدرك أنه ليس لدينا أي سيطرة على القرارات التي يتخذها شركاؤنا في معالجة الدفع، ولا يمكننا تحت أي ظرف من الظروف ضمان نجاح كل معاملة تقوم بها. إذا واجهت أي مشكلات تتعلق بمعالجة الدفع، فنحن نشجعك على الاتصال بفريق دعم العملاء لدينا للحصول على المساعدة، ولكن لا يمكننا تحمل أي مسؤولية عن المعاملات المرفوضة أو المرفوضة من قبل شركاء معالجة الدفع لدينا، ولن نكون مسؤولين عن تقديم أي مبالغ مستردة، دفع أي تعويضات أو اتخاذ المزيد من الإجراءات من أجل إغاثةكم.
    </li>
    
    <li>أنت تقر أيضًا بأن شركاء معالجة الدفع لدينا و/أو الشركات التابعة لجهات خارجية، قد يرفضون أي معاملة و/أو دفعة من السلطات القضائية المذكورة أدناه:
    
    <ol class="decimal space-y-4 ml-8">
    <li>كوبا</li>
    <li>إيران</li>
    <li>كوريا الشمالية</li>
    <li>سوريا</li>
    <li>روسيا</li>
    <li>شبه جزيرة القرم</li>
    </ol>
    </li>
    
    <p>
    نحن لا نتحمل أي مسؤولية على الإطلاق عن أي معاملات و/أو مدفوعات تم رفضها أو رفضها أو حظرها أو تجميدها من قبل شركاء معالجة الدفع لدينا و/أو الشركات التابعة لأطراف ثالثة، ولن يكون لديك أي مطالبات ضدنا في هذا الصدد.
     </p>
     
    <li>
    نحن نبذل العناية المعقولة، إلى الحد الذي يقع ضمن سيطرتنا، لضمان أمان طلبك ومعلومات الدفع الخاصة بك. ومع ذلك، لا يمكننا أن نتحمل المسؤولية عن أي خسارة قد تتعرض لها إذا تمكن طرف ثالث من الوصول غير المصرح به إلى أي بيانات تقدمها لنا أثناء الوصول إلى الموقع الإلكتروني أو الطلب منه، باستثناء حالات الإهمال المؤكد من جانبنا.
    </li>
    
    <li>
    المنتجات مقومة بالدرهم الإماراتي، ومع ذلك، عند شراء منتج باستخدام بطاقة خصم أو بطاقة ائتمان خارج دولة الإمارات العربية المتحدة، سيتم احتساب السعر النهائي المطلوب منك دفعه وفقًا لسعر الصرف المطبق في الوقت الذي تقوم فيه جهة إصدار بطاقتك بمعالجة المعاملة. نحن لا نتحمل أي مسؤولية على الإطلاق فيما يتعلق بسعر الصرف الذي قد يفرضه عليك مصدر بطاقتك مقابل هذه المعاملات.
    </li>
    <li>
    لمتابعة أي عمليات شراء، يتعين عليك التسجيل في الموقع. من خلال التسجيل، فإنك توافق على الحفاظ على سرية كلمة المرور الخاصة بك وستكون المسؤول الوحيد عن جميع الأنشطة التي تتم باستخدام حسابك وكلمة المرور الخاصة بك.
    </li>
    
    <li>
    للتسجيل والاحتفاظ بحساب معنا، فإنك تؤكد أنك تستوفي جميع المعايير التالية:
      <ol class="decimal space-y-4 ml-8">
      <li>أن يكون عمرك 18 (ثمانية عشر) عامًا على الأقل.</li>
      <li>ليس لديك حساب معنا بالفعل.</li>
      <li>أنت لست مقيمًا في دولة خاضعة للعقوبات أو الحظر.</li>
      <li>أنت لا تظهر في أي قائمة عالمية للأفراد ذوي المخاطر العالية والمعاقبين.</li>
      <li>لا يُحظر عليك أو يُمنع عليك الوصول إلى الموقع الإلكتروني، أو إنشاء حسابك المسجل أو الحفاظ عليه، أو الدخول في السحب بموجب القوانين واللوائح ذات الصلة في الولاية القضائية التي تتواجد فيها عند التعامل معنا.</li>
      </ol>
    </li>
    
    <li>يتعين عليك إخطارنا فورًا بأي خرق أمني أو استخدام غير مصرح به لحسابك. على الرغم من أننا لن نكون مسؤولين عن أي خسائر متكبدة بسبب الاستخدام غير المصرح به لحسابك، فقد تكون مسؤولاً عن أي خسائر قد نتكبدها نحن أو الآخرون نتيجة لهذا الاستخدام غير المصرح به. بالإضافة إلى ذلك، نحتفظ بالحق في إزالة اسم المستخدم الذي اخترته أو استعادته أو تعديله إذا رأينا أنه غير لائق أو فاحش أو مرفوض بأي شكل آخر، وفقًا لتقديرنا الخاص.
    </li>
    <li>من خلال التسجيل، فإنك تقر وتضمن أن جميع معلومات التسجيل التي تقدمها أو قدمتها صحيحة ودقيقة وحديثة وكاملة. أنت توافق على الحفاظ على دقة هذه المعلومات وتحديث معلومات التسجيل هذه على الفور حسب الضرورة. علاوة على ذلك، فإنك تؤكد أن لديك الأهلية القانونية للتسجيل وأنك ستلتزم بهذه الشروط والأحكام في جميع الأوقات.
    </li>
    </ol>

</li>








<li ><strong>نموذج الاشتراك</strong>
<ol class="decimal space-y-8 ml-8">
<li>نحن نقدم لك خيار أن تصبح مشتركًا في خدمتنا، أثناء عملية تسجيل الدخول إلى حسابك، وهو أمر متروك لك تمامًا. أنت غير ملزم بالاشتراك، وقرار القيام بذلك متروك لك تمامًا.
</li>
<li>من خلال الاشتراك في عرض الاشتراك المحدد الذي نقدمه، سيتم تسجيلك وتحصيل الرسوم منك على أساس شهري لعدد محدد من المنتجات، وسيمنحك كل منتج العدد المناسب من الإدخالات للمشاركة في السحوبات القادمة التي تختارها، أثناء الاشتراك فترة.
</li>
<li>
أنت تقر وتوافق على أن كل مسابقة سحب شهرية ستتميز بجوائز وقواعد محددة، والتي سيتم إبلاغك بها قبل كل سحب، وتوافق على الالتزام بقواعد مسابقة السحب وتفهم أن أي انتهاك قد يؤدي إلى حقوقنا وفقًا لما هو منصوص عليه في البند 11.11.
</li>
<li>
من خلال تحديد خيار اشتراك الدفع المتكرر على موقعنا، فإنك تقر وتوافق على التسجيل في طلب تجديد تلقائي وتوافق على تحصيل رسوم الاشتراك الشهري المتكرر المحددة أثناء تسجيل اشتراكك.
</li>
<li>
من خلال الاشتراك في موقعنا، فإنك تسمح لنا بتحصيل رسوم الاشتراك الشهري المطبقة على طريقة الدفع التي قدمتها أثناء تسجيل اشتراكك، ما لم تقم بإلغاء الاشتراك قبل تاريخ التجديد.
</li>
<li>
سيتم فرض رسوم الاشتراك المتكررة كل شهر، في التاريخ المشار إليه أثناء عملية تسجيل الدخول.
</li>
<li>
لديك الحق في إلغاء اشتراكك المتكرر في أي وقت. للإلغاء، يجب عليك اتباع عملية الإلغاء الموضحة على موقعنا الإلكتروني أو الاتصال بفريق دعم العملاء لدينا. أنت تقر وتوافق على أن الإلغاءات ستكون سارية اعتبارًا من شهر الفاتورة التالي بعد استلام طلب الإلغاء.
</li>
<li>
رسوم الاشتراك غير قابلة للاسترداد، ونحن نحتفظ بالحق في عدم تقديم أي مبالغ مستردة أو أرصدة لفترات الاشتراك المستخدمة جزئيًا.
</li>
<li>
تقع على عاتقك مسؤولية مطلقة الحفاظ على معلومات دفع دقيقة وحديثة في حسابك. إذا فشلت طريقة الدفع الخاصة بك أو تعذر تجديد اشتراكك بسبب تفاصيل الدفع غير الصالحة أو القديمة، فقد نقوم بتعليق اشتراكك حتى يتم تحديث معلومات الدفع. لن نتحمل المسؤولية، ولسنا ملزمين بإخطارك، عن أي انقطاع أو انقطاع في الخدمات المقدمة من قبلنا والتي قد تحدث نتيجة للظروف الموضحة في هذا البند 11.10.
</li>
<li>
نحن نحتفظ بالحق في تعليق أو استبعاد أي مسابقة سحب أو إنهاء اشتراكك المتكرر في حالة عدم الدفع، أو عدم الامتثال للفقرة 11.3، أو انتهاك هذه الشروط والأحكام، أو أي خرق آخر للاتفاقية. في مثل هذه الحالات، سيتم إعلامك عبر البريد الإلكتروني أو من خلال الموقع الإلكتروني، وستتاح لك الفرصة لحل المشكلة، إن أمكن.
</li>
</ol>
</li>




<li ><strong>ORDERS</strong>
<p>
عادةً ما تكون المنتجات التي نعرضها للبيع متوفرة في المخزون ومتاحة للإرسال وفقًا للبند 4.3 أعلاه. ومع ذلك، في مناسبات معينة، قد نكون في انتظار الشحنات من موردينا. يرجى ملاحظة أنه قد تكون هناك حالات لا نتمكن فيها من الالتزام فورًا بالبند 4.3 بسبب مشكلات الإنتاج أو مشكلات فحص الجودة التي تم تحديدها عند استلام المنتجات من موردينا. في مثل هذه الحالات، سنخطرك على الفور عبر البريد الإلكتروني، وفي غضون (30) ثلاثين يومًا من إبلاغنا بأن المنتجات غير متوفرة وأننا غير قادرين على الالتزام بالبند 4.3، سنعيد أي مبلغ دفعته مقابل المنتجات غير المتوفرة و لن تعد تذكرة دخول الموضوع إلى السحب القادم صالحة.
</p>
</li>



<li ><strong>التسعير</strong>
<ol class="decimal space-y-8 ml-8">
<li>
الأسعار المعروضة لمنتجاتنا على الموقع الإلكتروني مقومة بالدرهم الإماراتي (درهم الإمارات العربية المتحدة) وهي لا تشمل أي ضرائب مطبقة.
</li>
<li>
أنت تقر وتوافق على أن الأسعار المعروضة لمنتجاتنا قابلة للتغيير في أي وقت، وفقًا لتقديرنا الخاص.
</li>
</ol>
</li>


<li ><strong>قبول الطلب</strong>
<ol class="decimal space-y-8 ml-8">
<li>
بمجرد تقديم طلبك، ستتلقى رسالة بريد إلكتروني تُعلمك بتفاصيل طلبك. يعد هذا البريد الإلكتروني بمثابة إقرار باستلام طلبك ولا ينبغي اعتباره بمثابة قبول لطلبك.
</li>
<li>
نحن نحتفظ بالحق المطلق في رفض الطلب للأسباب التالية:
<ol class="decimal space-y-8 ml-12">
  <li>
  في الحالات التي يكون فيها مزود (مقدمو) الدفع لدينا غير قادرين على الحصول على ترخيص لدفعتك؛
  </li>
  <li>
  إذا كان شراؤك للمنتجات ينتهك أي شرط من هذه الشروط والأحكام؛
  </li>
  <li>
  إذا كان لدينا سبب للشك في أن الطلب قد تم تقديمه عن طريق الاحتيال أو أنه ينتهك أي قانون أو لائحة معمول بها؛ أو
  </li>
  <li>
  عندما نقرر أن هذا الرفض ضروري لأي سبب وجيه آخر.
  </li>
</ol>
</li>

<li>
إن قرارنا برفض الطلب يقع ضمن تقديرنا الخاص ولا ينشئ أي التزام بتقديم تفسيرات مفصلة لمثل هذه الإجراءات.
</li>
<li>
إذا رفضنا شراء منتج ما، فسنبلغك على الفور بالرفض، وسيتم رد أي مبلغ تم دفعه مقابل الطلب إليك في أقرب وقت ممكن. ستتم معالجة المبالغ المستردة، وفقًا للبند 14.2، باستخدام نفس طريقة الدفع التي استخدمتها أثناء الشراء الأولي للمنتج (المنتجات).
</li>
<li>
سيتم اعتبار قبول طلبك وإتمام العقد المبرم بينك وبيننا نهائيًا بمجرد استلام تذكرة الدخول إلى السحب القادم. وهذا يؤكد أن عملية الشراء الخاصة بك قد تمت معالجتها بنجاح، وأنك قد دخلت رسميًا في السحب القادم.
</li>
</ol>
</li>


<li ><strong>مواقع الطرف الثالث والمحتوى</strong>
<ol class="decimal space-y-8 ml-8">
<li>
قد يحتوي موقع الويب (أو قد يتم إرسالك عبر موقع الويب) على روابط لمواقع ويب أخرى ("مواقع الطرف الثالث") بالإضافة إلى مقالات وصور فوتوغرافية ونصوص ورسومات وصور وتصميمات وموسيقى وصوت وفيديو ومعلومات وتطبيقات والبرامج والمحتويات أو العناصر الأخرى التي تنتمي إلى أطراف ثالثة أو تنشأ منها ("محتوى الطرف الثالث").
</li>
<li>
لا نقوم بالتحقيق في مواقع الطرف الثالث ومحتوى الطرف الثالث أو مراقبتها أو التحقق من دقتها أو ملاءمتها أو اكتمالها، ونحن لسنا مسؤولين عن أي مواقع ويب تابعة لجهات خارجية يتم الوصول إليها من خلال موقع الويب أو أي محتوى لطرف ثالث منشور على ، المتوفرة من خلال موقع الويب أو المثبتة منه، بما في ذلك المحتوى أو الدقة أو الهجوم أو الآراء أو الموثوقية أو ممارسات الخصوصية أو السياسات الأخرى أو الواردة في مواقع الطرف الثالث أو محتوى الطرف الثالث.
</li>
<li>
إن تضمين أو ربط أو السماح باستخدام أو تثبيت أي مواقع ويب تابعة لجهات خارجية أو أي محتوى خاص بطرف ثالث لا يعني موافقتنا أو تأييدنا لها. إذا قررت مغادرة موقع الويب والوصول إلى مواقع الطرف الثالث أو استخدام أو تثبيت أي محتوى لطرف ثالث، فإنك تفعل ذلك على مسؤوليتك الخاصة، ويجب أن تدرك أن هذه الشروط والأحكام لم تعد سارية.
</li>
</ol>
</li>






<li ><strong>مجموعة</strong>
<ol class="decimal space-y-8 ml-8">
<li>
عند الفوز في مسابقة السحب، سيتم إبلاغ الفائز عبر البريد الإلكتروني إلى عنوان البريد الإلكتروني المسجل الذي قدمه المشارك أثناء عملية تسجيل الدخول، بما في ذلك تعليمات حول كيفية المطالبة بالجائزة.
</li>
<li>
سيكون الفائز في مسابقة السحب، سواء كان مقيمًا داخل دولة الإمارات العربية المتحدة أو خارجها، مسؤولاً عن استلام الجائزة شخصيًا من مقرنا خلال 60 (ستين) يومًا تقويميًا. للقيام بذلك، يجب على الفائز تقديم وثيقة هوية صالحة: إما بطاقة هوية إماراتية للفائز (الفائزين) المقيمين في دولة الإمارات العربية المتحدة أو جواز سفر وطني للفائز (الفائزين) الدوليين. بالإضافة إلى ذلك، يجب على الفائز أيضًا تقديم دليل يؤكد ملكية الحساب الفائز، وأي مستند آخر قد نطلبه، وفقًا لتقديرنا المطلق. ينطبق هذا البند على جميع أنواع الجوائز، بما في ذلك على سبيل المثال لا الحصر، المركبات والنقود.
</li>
<li>
لدينا الحق في سحب الجائزة من أي فائز لا يقدم نموذجًا صالحًا لإثبات الهوية الرسمية أو إثبات الحساب ولن نكون مسؤولين عن هذا السحب. لا يجوز التنازل عن الجائزة أو نقلها إلى أي طرف ثالث، سواء كان عائلة أو شركة، ولا يمكن المطالبة بالجائزة إلا من قبل الفائز الرسمي.
</li>
<li>
إذا لم يتم استلام الجائزة خلال الإطار الزمني المحدد، كما هو مذكور في البند 16.2 أعلاه، فإننا نحتفظ بالحق في تحويل الجائزة غير المطالب بها إلى دائرة الاقتصاد والسياحة، إدارة التفتيش. بعد النقل، لن نتحمل أي مسؤولية أو التزام آخر فيما يتعلق بالجائزة. ويُنصح المشاركون بالالتزام بفترة التحصيل المحددة لتجنب مصادرة الجائزة. بمجرد قيامنا بتحويل الجائزة غير المطالب بها إلى دائرة الاقتصاد والسياحة، قسم التفتيش، يصبح الفائز وحده مسؤولاً عن بدء التواصل معه والالتزام بأي إجراءات تحددها الإدارة المختصة للمطالبة بالجائزة غير المطالب بها، إن وجدت.
</li>
<li>
عند استلام الجائزة، سيُطلب من الفائز التقاط صورة فوتوغرافية بجانب الجائزة وممثلنا المعتمد لأغراض ترويجية. من خلال اختيار شراء منتجنا (منتجاتنا) والمشاركة لاحقًا في مسابقة السحب، فإنك تمنح طوعًا موافقة صريحة لنشر هذه المواد التسويقية على شبكة التواصل الاجتماعي الرسمية الخاصة بنا، ولا يلزم الحصول على موافقة إضافية أو استباقية بخلاف موافقتك الأولية الموافقة على هذه الشروط والأحكام.
</li>
</ol>
</li>


<li ><strong>بيانات شخصية</strong>
<ol class="decimal space-y-8 ml-8">
<li>
نحن نعطي الأولوية لخصوصية وأمان بياناتك الشخصية. نحن نحثك على مراجعة سياسة الخصوصية الخاصة بنا، والتي يمكن الوصول إليها على الموقع. باستخدام الموقع، فإنك تقر وتوافق صراحةً على الالتزام بسياسة الخصوصية الخاصة بنا، لأنها تشكل جزءًا أساسيًا من هذه الشروط والأحكام. يشير استمرار استخدامك للموقع إلى موافقتك على الالتزام بالشروط والأحكام الموضحة في سياسة الخصوصية.
</li>
<li>
يرجى العلم أن الموقع يعمل ويتم استضافته في دولة الإمارات العربية المتحدة، وبالتالي قد تتم معالجة بياناتك الشخصية وتخزينها وفقًا لقوانين ولوائح دولة الإمارات العربية المتحدة.
</li>
<li>
من خلال الدخول في السحب، فإنك توافق بشكل لا رجعة فيه على نشر بياناتك عبر القنوات الإعلامية وموقعنا الإلكتروني وأي منصات عامة أخرى، وفقًا لهذا البند 17.3 والبند 16.5 أعلاه. كشرط بموجب الشروط ذات الصلة للدخول في السحب، سيتم إدراج اسمك علنًا على موقعنا ووسائل الإعلام وقنوات التواصل الاجتماعي. يتضمن ذلك حقنا في نشر صور استلام الجائزة وأي وسائط أخرى مطلوبة لضمان الإعلان عن الفائزين علنًا وفقًا لمتطلبات دائرة الاقتصاد والسياحة في دبي في دولة الإمارات العربية المتحدة.

</li>
<li>
إذا اخترت سحب موافقتك على استخدام بياناتك، بما في ذلك على سبيل المثال لا الحصر، اسمك وصور استلام الجائزة، عبر مختلف المنصات الرقمية مثل مواقع الويب وشبكات التواصل الاجتماعي، فيجب عليك تقديم طلب كتابي لنا وسنشرع في إزالة بياناتك الشخصية وصورك الفوتوغرافية من أي سياق رقمي تم نشرها فيه.

</li>
<li>
من خلال الدخول في السحب، فإنك توافق تمامًا على أنه قد يتم تقديم التفاصيل الخاصة بك إلى أطراف ثالثة وفقًا لما يقتضيه القانون، وقد يكون من الضروري تقديم هذه التفاصيل من أجل الامتثال للمتطلبات المنصوص عليها في التشريعات ذات الصلة من أجل ضمان التطبيق الكامل. وتنفيذ السحب .
</li>
</ol>
</li>





<li ><strong>المدة والإنهاء</strong>
<ol class="decimal space-y-8 ml-8">
<li>
تظل هذه الشروط والأحكام سارية المفعول والتأثير بالكامل أثناء استخدامك للموقع. دون تقييد أي بند آخر من هذه الشروط والأحكام، نحن نحتفظ بالحق، وفقًا لتقديرنا الخاص ودون إشعار أو مسؤولية، في رفض الوصول إلى الموقع واستخدامه (بما في ذلك حظر عناوين IP معينة)، لأي شخص لأي سبب أو دون أي سبب، بما في ذلك، على سبيل المثال لا الحصر، انتهاك أي إقرار أو ضمان أو ميثاق وارد في هذه الشروط والأحكام أو أي قانون أو لائحة معمول بها. يجوز لنا إنهاء استخدامك أو مشاركتك في الموقع أو حذف حسابك وأي محتوى أو معلومات قمت بنشرها في أي وقت، دون سابق إنذار، وفقًا لتقديرنا وحدنا.

</li>
<li>
لا يشكل إنهاء أو تقييد وصولك أو استخدامك للموقع تنازلاً أو يؤثر على أي حقوق أو تعويضات أخرى قد يحق لنا الحصول عليها بموجب القانون المعمول به. حقوقنا وسبل الانتصاف الخاصة بنا بموجب هذه الشروط والأحكام هي حقوق تراكمية بالإضافة إلى أي حقوق وسبل انتصاف أخرى ينص عليها القانون.
</li>
</ol>
</li>



<li ><strong>التعديلات والانقطاعات</strong>
<ol class="decimal space-y-8 ml-8">
<li>
نحن نحتفظ بالحق، وفقًا لتقديرنا الخاص، في تغيير أو تعديل أو إيقاف أو إزالة أي محتوى على الموقع دون إشعار مسبق. في حالة حدوث أي تغييرات أو تعديلات أو توقف، لن نتحمل المسؤولية تجاهك أو تجاه أي طرف ثالث عن أي عواقب ناتجة، بما في ذلك على سبيل المثال لا الحصر التعديلات في المحتوى أو التسعير أو التعليق أو إنهاء الموقع.
</li>
<li>
بينما نبذل قصارى جهدنا للحفاظ على توفر الموقع، لا يمكننا ضمان الوصول دون انقطاع في جميع الأوقات. من وقت لآخر، قد نواجه مشكلات في الأجهزة أو البرامج أو غيرها من المشكلات الفنية، أو قد نحتاج إلى إجراء صيانة قد تؤدي إلى انقطاع أو تأخير أو أخطاء في الوصول إلى الموقع.
</li>

<li>
في حالة إنهاء أو تعليق وصولك إلى الموقع، فإنك تقر وتوافق على أنك ستظل ملتزمًا بهذه الشروط والأحكام. أنت تفهم وتقبل أيضًا أننا لن نتحمل المسؤولية عن أي خسارة أو ضرر أو إزعاج ينشأ عن عدم قدرتك على الوصول إلى الموقع أو استخدامه خلال فترات التوقف أو التوقف.
</li>
</ol>
</li>





<li ><strong>القانون الذي يحكم</strong>
<ol class="decimal space-y-8 ml-8">
<li>
تحكم قوانين دولة الإمارات العربية المتحدة المطبقة في إمارة دبي هذه الشروط والأحكام ووصولك إلى موقعنا الإلكتروني، دون تفعيل أي مبادئ تعارض القوانين الواردة فيها.
</li>
<li>
أي نزاع يتعلق بهذه الشروط والأحكام أو تعاملنا مع معلوماتك الشخصية والمعلومات العامة يخضع للاختصاص القضائي الحصري للمحاكم في دبي.
</li>
</ol>
</li>




<li ><strong>تنصل</strong>
<ol class="decimal space-y-8 ml-8">
<li>
يتم توفير موقع الويب كما هو وكما هو متاح. أنت توافق على أن استخدامك لخدمات الموقع سيكون على مسؤوليتك وحدك. إلى أقصى حد يسمح به القانون، نحن نخلي مسؤوليتنا من جميع الضمانات، الصريحة أو الضمنية، فيما يتعلق بالموقع واستخدامك له، بما في ذلك، على سبيل المثال لا الحصر، الضمانات الضمنية لقابلية التسويق والملاءمة لغرض معين والخلو من فيروسات الكمبيوتر. وغير -الانتهاك. نحن لا نقدم أي ضمانات أو تعهدات بشأن دقة أو اكتمال محتوى الموقع أو محتوى أي مواقع ويب مرتبطة بهذا الموقع ولن نتحمل أي مسؤولية أو مسؤولية عن أي (1) أخطاء أو أخطاء أو عدم دقة في المحتوى الخيمة والمواد ( 2) الإصابة الشخصية أو الأضرار بالممتلكات، من أي نوع كانت، الناتجة عن وصولك إلى الموقع واستخدامه، (3) أي وصول غير مصرح به أو استخدام لخوادمنا الآمنة و/أو أي وجميع المعلومات الشخصية و/أو المعلومات المالية مخزنة فيه، (4) أي انقطاع أو توقف للإرسال إلى موقع الويب أو منه، (5) أي أخطاء أو فيروسات أو أحصنة طروادة أو ما شابه ذلك يمكن نقلها إلى موقع الويب أو من خلاله بواسطة أي طرف ثالث و/أو ( 6) أي أخطاء أو سهو في أي محتوى ومواد أو أي خسارة أو ضرر من أي نوع يحدث نتيجة لاستخدام أي محتوى منشور أو منقول أو بخلاف ذلك متاح عبر الموقع الإلكتروني. نحن لا نضمن أو نؤيد أو نضمن أو نتحمل المسؤولية عن أي منتج أو خدمة يتم الإعلان عنها أو تقديمها من قبل طرف ثالث من خلال موقع الويب، أو أي موقع ويب مرتبط تشعبيًا، أو أي موقع ويب أو تطبيق جوال يظهر في أي لافتة أو إعلانات أخرى ونحن لن نفعل ذلك كن طرفًا أو مسؤولاً بأي شكل من الأشكال عن مراقبة أي معاملة بينك وبين أي من مقدمي المنتجات أو الخدمات من الجهات الخارجية. كما هو الحال مع شراء منتج أو خدمة من خلال أي وسيلة أو في أي بيئة، يجب عليك استخدام أفضل حكم لديك وتوخي الحذر حيثما كان ذلك مناسبًا.
</li>
</ol>
</li>



<li ><strong>التعويض</strong>
<ol class="decimal space-y-8 ml-8">
<li>
أنت توافق بموجب هذا على الدفاع عنا وتعويضنا وحمايتها، نحن الشركات التابعة لنا، والشركات الشريكة، والشركات التابعة، والشركات الأم أو الشقيقة، بالإضافة إلى مسؤوليها، ومديريها، ومساهميها، وشركائها، وخلفائها، والمتنازل لهم، ووكلائها، ومقدمي الخدمات، والموردين، والموظفين، من وضد أي وجميع المطالبات والأضرار والالتزامات والخسائر (سواء كانت مباشرة أو غير مباشرة أو تبعية)، والإصابات، والالتزامات، والتكاليف، أو الديون، والنفقات (بما في ذلك على سبيل المثال لا الحصر الرسوم القانونية) الناشئة عن أو ذات الصلة ل:

<ol class="alpha space-y-8 ml-6 sm:ml-8">
<li>
انتهاكك لأي بند ضمن هذه الشروط والأحكام و/أو أي بند آخر لأي وثيقة أو سياسة أو اتفاقية صادرة عنا ومنشورة على الموقع الإلكتروني أو مقدمة لك عبر وسائل أخرى.
</li>
<li>
انتهاكك لأي حقوق لأطراف ثالثة، بما في ذلك، على سبيل المثال لا الحصر، حقوق الطبع والنشر أو العلامات التجارية أو الأسرار التجارية أو حقوق الملكية الفكرية الأخرى أو حقوق الخصوصية.
</li>
<li>
استخدامك للموقع والحساب المسجل وخدماتنا.
</li>
<li>
أي مطالبة تؤكد أن موقعنا الإلكتروني أو منتجاتنا أو خدماتنا أو جوائزنا قد أدت إلى إلحاق الضرر بك أو بطرف ثالث.
</li>
</ol>
</li>
<li>
ويظل التزام الدفاع والتعويض هذا ساريًا بعد إنهاء أو تعديل أو انتهاء هذه الشروط والأحكام واستخدامك لخدماتنا والموقع الإلكتروني. ومن المفهوم أن التزام التعويض هذا يظل ساريًا وملزمًا، ويحمي مصالح الطرفين بعد انتهاء الشروط أو الأحكام المذكورة.
</li>
</ol>
</li>





<li ><strong>حدود المسؤولية</strong>
<ol class="decimal space-y-8 ml-8">
<li>
لا يجوز لنا نحن أو كيانات مجموعتنا أو الشركات التابعة أو التابعة لنا أو مديرينا أو موظفينا أو أعضائنا أو شركائنا أو موردينا أو وكلائنا القيام بما يلي:


<ol class="alpha space-y-8  ml-6 sm:ml-8  ">
<li>
نكون مسؤولين تجاهك أو تجاه أي طرف ثالث عن أي أضرار مباشرة أو غير مباشرة أو تبعية أو نموذجية أو عرضية أو خاصة أو تأديبية تنشأ عن استخدامك للموقع. قد تشمل هذه الأضرار، على سبيل المثال لا الحصر، خسارة الأرباح أو الإيرادات المفقودة أو فقدان البيانات أو أي شكل آخر من أشكال الخسارة أو الضرر، بغض النظر عما إذا تم إخطارنا بإمكانية حدوث مثل هذه الأضرار، وعلى الرغم مما سبق ذكره، لا توجد قيود المسؤولية إلى الحد الذي لا يسمح به القانون المعمول به؛
</li>
<li>
كن مسؤولاً عن أي انتهاك أو نزاع بشأن المعاملات، بما في ذلك على سبيل المثال لا الحصر، نزاعات المعاملات المتعلقة بالجودة والسلامة وخرق الضمان والدفع بواسطتك؛
</li>
<li>
نكون مسؤولين عن أي أضرار ناجمة عن مشاكل أو أعطال فنية لأي شبكة أو خطوط هاتفية، أو أنظمة الكمبيوتر عبر الإنترنت، أو الخوادم أو مقدمي الخدمة، أو معدات الكمبيوتر، أو البرامج، أو الأجهزة، أو أي فشل في الاتصالات بسبب مشاكل فنية أو ازدحام حركة المرور على الإنترنت، سواء على موقع الكتروني أو غير ذلك. يتضمن ذلك أي إصابة أو ضرر يلحق بك أو بجهاز الكمبيوتر الخاص بك أو بجهاز كمبيوتر أي شخص آخر يتعلق باستخدام الموقع أو ناتج عنه.

</li>
<li>
بالتزامن مع قيود الضمانات كما هو موضح هنا، فإنك تفهم وتوافق صراحةً على أن الحد الأقصى لمسؤوليتنا عن أي مطالبة ضدنا، تنشأ عن استخدامك لموقعنا الإلكتروني و/أو خدماتنا، يجب أن يقتصر بشكل صارم على مبلغ 100 درهم إماراتي (واحد مائة درهم إماراتي). ينطبق هذا القيد على المسؤولية على جميع المطالبات أو الأضرار أو الخسائر أو النفقات، بما في ذلك على سبيل المثال لا الحصر، الأضرار المباشرة أو غير المباشرة أو العرضية أو التبعية أو التأديبية، ويمتد إلى أي نظرية قانونية أو سبب الدعوى.

</li>
</ol>
</li>
<li>
ويظل التزام الدفاع والتعويض هذا ساريًا بعد إنهاء أو تعديل أو انتهاء هذه الشروط والأحكام واستخدامك لخدماتنا والموقع الإلكتروني. ومن المفهوم أن التزام التعويض هذا يظل ساريًا وملزمًا، ويحمي مصالح الطرفين بعد انتهاء الشروط أو الأحكام المذكورة.
</li>
</ol>
</li>


<li ><strong>متنوع</strong>
<ol class="decimal space-y-8 ml-6 sm:ml-8 ">
<li>
تشكل هذه الشروط والأحكام وأي سياسات أو قواعد تشغيل منشورة من قبلنا على الموقع الإلكتروني أو فيما يتعلق بالموقع الاتفاق والتفاهم الكامل بينك وبيننا. إن فشلنا في ممارسة أو إنفاذ أي حق أو شرط من هذه الشروط والأحكام لا يشكل تنازلاً عن هذا الحق أو الحكم.
</li>
<li>
تعمل هذه الشروط والأحكام إلى أقصى حد يسمح به القانون. يجوز لنا التنازل عن أي أو كل حقوقنا والتزاماتنا للآخرين في أي وقت. لن نكون مسؤولين أو مسؤولين عن أي خسارة أو ضرر أو تأخير أو فشل في التصرف لأي سبب خارج عن سيطرتنا المعقولة.
</li>
<li>
لن نتحمل المسؤولية عن أي تأخير أو فشل أو انقطاع في الخدمات التي نقدمها لك من خلال موقعنا، أو عن أي انقطاع يؤثر على الموقع نفسه، والذي ينشأ بشكل مباشر أو غير مباشر بسبب أعمال الطبيعة أو القوى أو أسباب خارجة عن سيطرتنا. تشمل هذه الأسباب، على سبيل المثال لا الحصر، فشل الإنترنت، أو تعطل أنظمة الكمبيوتر أو الاتصالات، أو فشل المعدات، أو انقطاع الطاقة الكهربائية، أو الإضرابات العمالية، أو النزاعات العمالية، أو أعمال الشغب، أو التمرد، أو الاضطرابات المدنية، أو نقص العمالة أو المواد، أو تفشي الأوبئة أو الجوائح. أو الحرائق أو الفيضانات أو العواصف أو الانفجارات أو القضاء والقدر أو أعمال الحرب أو الإجراءات التي تتخذها السلطات الحكومية أو الأوامر الصادرة عن المحاكم المحلية أو الأجنبية أو عدم الأداء من قبل أطراف ثالثة. سنبذل قصارى جهدنا للتخفيف من تأثير مثل هذه الأحداث ونسعى جاهدين لاستعادة الحياة الطبيعية في أقرب وقت ممكن عمليًا. ومع ذلك، لن نتحمل المسؤولية عن أي أضرار أو خسائر أو مضايقات تنشأ عن مثل هذه الأحداث التي تكون خارجة عن سيطرتنا المعقولة.
</li>
<li>
إذا تم تحديد أي بند أو جزء من بند من هذه الشروط والأحكام على أنه غير قانوني أو باطل أو غير قابل للتنفيذ، فإن هذا الحكم أو جزء من الحكم يعتبر قابلاً للفصل عن هذه الشروط والأحكام ولا يؤثر على صحة وقابلية تنفيذ أي من الشروط والأحكام المتبقية. أحكام. لا يوجد مشروع مشترك أو شراكة أو توظيف أو علاقة وكالة تنشأ بينك وبيننا نتيجة لهذه الشروط والأحكام أو استخدام الموقع.
</li>
</ol>
</li>


<li ><strong>اتصل بنا</strong>
<ol class="decimal space-y-8  ltr:ml-8 rtl:mr-8">
<li>
من أجل حل شكوى بخصوص موقع الويب أو للحصول على مزيد من المعلومات بخصوص استخدام موقع الويب، يرجى الاتصال بفريق خدمة العملاء لدينا على <a href="mailto:contact@winnar.com" class="text-primary" >contact@ winnar.com</a> أو من خلال حساباتنا على وسائل التواصل الاجتماعي أو نموذج الاتصال الخاص بنا، كما هو موضح هنا.
</li>
</ol>
</li>













</ol>

</div>
</div>
`;

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

  const reactElements = parse(
    storeBlogsData?.CMSDescription[lang.lang_id == 1 ? 0 : 1]?.content || '',
    {
      // const reactElements = parse(eventFaqs || '', {
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
                  'HTML CONTENT NOT FOUND',
              } as any
            }
          />
        </>
      )}
    </div>
  );
}
