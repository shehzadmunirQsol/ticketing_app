import dynamic from 'next/dynamic';
import React, { useEffect } from 'react';
import { prisma } from '~/server/prisma';
import parse from 'html-react-parser';
import AboutCarousel from '~/components/app/about/about_carousel';

export async function getStaticPaths() {
  const response: any = await prisma.cMS.findMany({
    select: {
      id: true,
      slug: true,
    },
  });
  console.log({ response }, 'get static path');

  const paths = response?.map((post: any) => ({
    params: { id: post?.slug },
  }));

  return { paths, fallback: false };
}
export async function getStaticProps({ params }: any) {
  console.log({ params }, 'params path');
  const response = await prisma.cMS.findFirst({
    where: {
      slug: params?.id,
    },

    select: {
      id: true,
      slug: true,
      type: true,
      CMSDescription: {
        select: {
          content: true,
        },
      },
    },
  });

  console.log({ response }, 'response data form static path');

  return { props: { storeBlogsData: response } };
}

// const content = `
// <div>
// <div className="w-full relative h-[750px] md:h-[650px] lg:h-[550px] z-40 text-center">
//   <Image
//     src="https://media.winnar.com/upload/about-page-background.png"
//     alt="/"
//     fill
//     quality={100}
//     className=" object-cover  w-full h-full block bg-black/50"
//   />
//   <div className="absolute h-[35px] w-full text-center top-[50%] flex items-center">
//     <div className="w-full text-center ">
//       <p className=" text-white drop-shadow-2xl text-center w-full text-4xl  lg:text-5xl tracking-tighter  uppercase font-[900]">
//         PRESTIGE CAR
//       </p>
//       <p className="relative h-[35px] -top-[50%]   mt-6 text-white drop-shadow-2xl text-center w-full text-4xl  lg:text-5xl tracking-tighter  uppercase font-[900]">
//         RAFFLES
//       </p>
//     </div>
//   </div>
// </div>

// <div>
//   <div className="bg-primary lg:w-[400px] md:w-[400px]  px-8 py-20 aboutus-card   mr-10 lg:ml-20 ml-5  mb-20">
//     <p className="font-black lg:text-5xl md:text-3xl text-xl text-black">Our Mission</p>
//     <p className="text-background pt-5 text-sm">
//       Winnar, the prestige car ra e company that o ers car enthusiasts the
//       thrilling opportunity to win their coveted dream car and many other
//       prizes. Through our captivating experiences and philanthropic
//       initiatives, we aim to create memorable moments that extend beyond
//       the thrill of winning! Join Winnar.com today and get ready to drive
//       away in the car of your dreams.
//     </p>
//   </div>
// </div>

// <div className="absolute top-[610px] right-64 px-8 xl:block hidden">
//   <svg
//     width="474"
//     height="895"
//     viewBox="0 0 674 1095"
//     fill="none"
//     xmlns="http://www.w3.org/2000/svg"
//   >
//     <path
//       d="M1.11379 547.5L486.887 1093.73H819.886L334.113 547.5H1.11379Z"
//       stroke="url(#paint0_linear_190_685)"
//     />
//     <path
//       d="M1.11379 546.728L486.887 0.5H819.886L334.113 546.728H1.11379Z"
//       stroke="url(#paint1_linear_190_685)"
//     />
//     <defs>
//       <linearGradient
//         id="paint0_linear_190_685"
//         x1="706"
//         y1="1094.23"
//         x2="220"
//         y2="531.228"
//         gradientUnits="userSpaceOnUse"
//       >
//         {/* <stop stop-opacity="0" /> */}
//         {/* <stop offset="1" stop-color="#454545" /> */}
//       </linearGradient>
//       <linearGradient
//         id="paint1_linear_190_685"
//         x1="706"
//         y1="-1.26162e-05"
//         x2="220"
//         y2="563"
//         gradientUnits="userSpaceOnUse"
//       >
//         {/* <stop stop-opacity="0" /> */}
//         {/* <stop offset="1" stop-color="#454545" /> */}
//       </linearGradient>
//     </defs>
//   </svg>
// </div>
//   <div data="main-carousel">
//     [
//         {
//             "img":"https://media.winnar.com/upload/founder.png",
//             "heading":"Scott L. Hughes",
//             "text":"Co-Founder",
//             "hoverhead":"Scott L. Hughes",
//             "hoverpera":"Co-Founder",
//             "hoverdesc":"Aenean vulputate eleifend tellus Aenean leo ligula porttitor eu consequat vitae eleifend ac enim. Aliquam lorem ante dapibus in viverra quis feugiat a tellus Phasellus viverra nulla ut metus varius laoreet Quisque rutrum"

//         },
//         {
//             "img":"https://media.winnar.com/upload/founder-1.png",
//             "heading":"Scott L. Hughes",
//             "text":"Founder",
//             "hoverhead":"Scott L. Hughes",
//             "hoverpera":"Founder",
//             "hoverdesc":"Aenean vulputate eleifend tellus Aenean leo ligula porttitor eu consequat vitae eleifend ac enim. Aliquam lorem ante dapibus in viverra quis feugiat a tellus Phasellus viverra nulla ut metus varius laoreet Quisque rutrum"

//         },
//         {
//             "img":"https://media.winnar.com/upload/founder-2.png",
//             "heading":"Ameen",
//             "text":"Founder",
//             "hoverhead":"Scott L. Hughes",
//             "hoverpera":"Founder",
//             "hoverdesc":"Aenean vulputate eleifend tellus Aenean leo ligula porttitor eu consequat vitae eleifend ac enim. Aliquam lorem ante dapibus in viverra quis feugiat a tellus Phasellus viverra nulla ut metus varius laoreet Quisque rutrum"

//         },
//         {
//             "img":"https://media.winnar.com/upload/founder-3.png",
//             "heading":"Muzi",
//             "text":"Founder",
//             "hoverhead":"Scott L. Hughes",
//             "hoverpera":"Founder",
//             "hoverdesc":"hello"

//         },
//         {
//             "img":"https://media.winnar.com/upload/founder.png",
//             "heading":"Ahmed",
//             "text":"Founder",
//             "hoverhead":"Scott L. Hughes",
//             "hoverpera":"Founder",
//             "hoverdesc":"hello"

//         },
//         {
//           "img":"https://media.winnar.com/upload/founder.png",
//           "heading":"Shehzad",
//           "text":"Founder",
//           "hoverhead":"Scott L. Hughes",
//           "hoverpera":"Founder",
//           "hoverdesc":"Aenean vulputate eleifend tellus Aenean leo ligula porttitor eu consequat vitae eleifend ac enim. Aliquam lorem ante dapibus in viverra quis feugiat a tellus Phasellus viverra nulla ut metus varius laoreet Quisque rutrum"

//       }
//     ]
// </div>
// <div className="w-full relative h-[650px] sm:h-[350px] md:h-[650px] lg:h-[550px] z-40 text-center">
//   <Image
//     src="https://media.winnar.com/upload/about-background-1.png"
//     alt="/"
//     fill
//     quality={100}
//     className=" object-cover w-full h-full  block bg-black/50"
//   />

//   <div className="absolute top-20 left-0 lg:left-72 px-8">
//     <div className=" text-center flex flex-col lg:flex-row gap-x-4 justify-center items-start font-sans lg:mx-auto">
//       <p className=" text-white drop-shadow-2xl text-center  font-black text-2xl  lg:text-3xl lg:w-fit">
//         OUR MVP
//       </p>
//       <div className=" font-normal max-w-3xl w-fit">
//         <p className=" text-white drop-shadow-2xl text-justify  ">
//           At Winnar, we offer discerning individuals an exclusive
//           opportunity to win their coveted dream car from our meticulously
//           curated collection of performance, luxury, and prestige brands.
//         </p>
//         <p className=" text-white drop-shadow-2xl text-xs text-justify  pt-10">
//           What sets us apart is our unwavering focus on transparency,
//           fairness, and excitement. Unlike traditional car purchasing
//           methods, Winnar.com provides an electrifying platform where
//           every participant is empowered to drive away in the car of their
//           dreams. With our transparent selection process, participants can
//           be confident that their chances are fair, and the thrill of
//           winning is unparalleled. Join us today to experience the epitome
//           of automotive aspirations and be a part of an exceptional ra e
//           journey.
//         </p>
//       </div>
//     </div>
//   </div>
// </div>

// <div className="w-full relative h-[250px] lg:h-[550px] z-40 text-center">
//   <Image
//     src="https://media.winnar.com/upload/about-background-2.png"
//     alt="/"
//     fill
//     quality={100}
//     className=" object-cover w-full  h-full block bg-black/50"
//   />
//   <div className="absolute h-[35px] w-full text-center top-[50%] flex items-center">
//     <div className="w-full text-center ">
//       <p className=" text-white drop-shadow-2xl text-center w-full text-xl  lg:text-3xl">
//         Get your dream car at a fraction of the price!
//       </p>
//       <p className="relative h-[35px] -top-[50%]   mt-6 text-white drop-shadow-2xl text-center w-full text-2xl  lg:text-4xl font-[900]">
//         WIN SUNDAY 8 PM
//       </p>
//       <Button
//         className=" px-16 mt-5 lg:mt-24 md:mt-24  bg-primary clip-style text-black font-sans font-[900] text-xl"
//         variant="default"
//       >
//         Enter Now
//       </Button>
//     </div>
//   </div>
// </div>
// </div>`

// const content = ` <section>
// <div class="w-full relative h-[750px] md:h-[650px] lg:h-[550px] z-40 text-center " >
//         <Image
//           src="https://media.winnar.com/upload/about-page-background.png"
//           alt="/"
//           fill
//           quality={100}
//           class=" object-cover  w-full h-full block bg-black/50"
//         />
// <div class="absolute h-[35px] w-full text-center top-[50%] flex items-center">
// <div class="w-full text-center">
// <p class="text-white drop-shadow-2xl text-center w-full text-4xl  lg:text-5xl tracking-tighter  uppercase font-[900]">FAQS</p>
// </div>
// </div>
// </div>


// <div className="absolute top-[40px] right-64 px-8 xl:block hidden">
//   <svg
//     width="474"
//     height="895"
//     viewBox="0 0 674 1095"
//     fill="none"
//     xmlns="http://www.w3.org/2000/svg"
//   >
//     <path
//       d="M1.11379 547.5L486.887 1093.73H819.886L334.113 547.5H1.11379Z"
//       stroke="url(#paint0_linear_190_685)"
//     />
//     <path
//       d="M1.11379 546.728L486.887 0.5H819.886L334.113 546.728H1.11379Z"
//       stroke="url(#paint1_linear_190_685)"
//     />
//     <defs>
//       <linearGradient
//         id="paint0_linear_190_685"
//         x1="706"
//         y1="1094.23"
//         x2="220"
//         y2="531.228"
//         gradientUnits="userSpaceOnUse"
//       >
//         {/* <stop stop-opacity="0" /> */}
//         {/* <stop offset="1" stop-color="#454545" /> */}
//       </linearGradient>
//       <linearGradient
//         id="paint1_linear_190_685"
//         x1="706"
//         y1="-1.26162e-05"
//         x2="220"
//         y2="563"
//         gradientUnits="userSpaceOnUse"
//       >
//         {/* <stop stop-opacity="0" /> */}
//         {/* <stop offset="1" stop-color="#454545" /> */}
//       </linearGradient>
//     </defs>
//   </svg>
// </div>



// <div class="relative  min-h-screen w-full flex flex-col gap-8  lg:px-14 md:px-14 px-4 mt-16">
// <div>




// <p class="lg:text-5xl md:text-4xl text-3xl text-start  font-black uppercase ">Frequently asked questions</p>

// <div class="border-b-4 w-16 border-primary mt-4">&nbsp;</div>
// <div class="relative py-8  min-h-screen w-full flex flex-col  gap-8 ">

// <div class="border-b border-t py-4 border-lightColorBorder hover:border-b-primary shadow-lg hover:shadow-2xl transition-all duration-300 overflow-hidden">
// <div class="group flex items-start py-4 transition duration-200">
// <div class="h-10 relative group-hover:h-full overflow-hidden transition-all duration-300">
// <p class="lg:text-3xl text-xl font-bold">Competition Rules</p>

// <p class="text-base mt-4 text-grayColor">Your ticket number(s) will be shown as soon as your order is confirmed and will be available under <span class="text-primary font-bold">&lsquo;My Account&rsquo;</span> and in your email confirmation.</p>
// </div>

// <div class="text-3xl group-hover:rotate-180 transition duration-500 ml-auto"><svg class="text-white group-hover:text-primary" fill="none" height="15" viewbox="0 0 24 15" width="24" xmlns="http://www.w3.org/2000/svg"> <path d="M2.82 0.159912L12 9.33991L21.18 0.159912L24 2.99991L12 14.9999L0 2.99991L2.82 0.159912Z" fill="currentColor" id="Vector"></path> </svg></div>
// </div>
// </div>

// <div class="border-b pb-4 border-lightColorBorder hover:border-b-primary  shadow-lg hover:shadow-2xl transition-all duration-300 overflow-hidden">
// <div class="group flex items-start py-4 transition duration-200">
// <div class="h-10 relative group-hover:h-full overflow-hidden transition-all duration-300">
// <p class="lg:text-3xl text-xl font-bold">When do I get my number?</p>

// <p class="text-base mt-4 text-grayColor">Your ticket number(s) will be shown as soon as your order is confirmed and will be available under <span class="text-primary font-bold">&lsquo;My Account&rsquo;</span> and in your email confirmation.</p>
// </div>

// <div class="text-3xl group-hover:rotate-180 transition duration-500 ml-auto"><svg class="text-white group-hover:text-primary" fill="none" height="15" viewbox="0 0 24 15" width="24" xmlns="http://www.w3.org/2000/svg"> <path d="M2.82 0.159912L12 9.33991L21.18 0.159912L24 2.99991L12 14.9999L0 2.99991L2.82 0.159912Z" fill="currentColor" id="Vector"></path> </svg></div>
// </div>
// </div>

// <div class="border-b pb-4 border-lightColorBorder hover:border-b-primary  shadow-lg hover:shadow-2xl transition-all duration-300 overflow-hidden">
// <div class="group flex items-start py-4 transition duration-200">
// <div class="h-10 relative group-hover:h-full overflow-hidden transition-all duration-300">
// <p class="lg:text-3xl text-xl font-bold">How do you do the draw?</p>

// <p class="text-base mt-4 text-grayColor">Your ticket number(s) will be shown as soon as your order is confirmed and will be available under <span class="text-primary font-bold">&lsquo;My Account&rsquo;</span> and in your email confirmation.</p>
// </div>

// <div class="text-3xl group-hover:rotate-180 transition duration-500 ml-auto"><svg class="text-white group-hover:text-primary" fill="none" height="15" viewbox="0 0 24 15" width="24" xmlns="http://www.w3.org/2000/svg"> <path d="M2.82 0.159912L12 9.33991L21.18 0.159912L24 2.99991L12 14.9999L0 2.99991L2.82 0.159912Z" fill="currentColor" id="Vector"></path> </svg></div>
// </div>
// </div>



// <div class="border-b pb-4  border-lightColorBorder hover:border-b-primary  shadow-lg hover:shadow-2xl transition-all duration-300 overflow-hidden">
// <div class="group flex items-start py-4 transition duration-200">
// <div class="h-10 relative group-hover:h-full overflow-hidden transition-all duration-300">
// <p class="lg:text-3xl text-xl font-bold">When is the draw?</p>

// <p class="text-base mt-4 text-grayColor">Your ticket number(s) will be shown as soon as your order is confirmed and will be available under <span class="text-primary font-bold">&lsquo;My Account&rsquo;</span> and in your email confirmation.</p>
// </div>

// <div class="text-3xl group-hover:rotate-180 transition duration-500 ml-auto"><svg class="text-white group-hover:text-primary" fill="none" height="15" viewbox="0 0 24 15" width="24" xmlns="http://www.w3.org/2000/svg"> <path d="M2.82 0.159912L12 9.33991L21.18 0.159912L24 2.99991L12 14.9999L0 2.99991L2.82 0.159912Z" fill="currentColor" id="Vector"></path> </svg></div>
// </div>
// </div>

// <div class="border-b pb-4 border-lightColorBorder hover:border-b-primary  shadow-lg hover:shadow-2xl transition-all duration-300 overflow-hidden">
// <div class="group flex items-start py-4 transition duration-200">
// <div class="h-10 relative group-hover:h-full overflow-hidden transition-all duration-300">
// <p class="lg:text-3xl text-xl font-bold">How is the winner chosen?</p>

// <p class="text-base mt-4 text-grayColor">Your ticket number(s) will be shown as soon as your order is confirmed and will be available under <span class="text-primary font-bold">&lsquo;My Account&rsquo;</span> and in your email confirmation.</p>
// </div>

// <div class="text-3xl group-hover:rotate-180 transition duration-500 ml-auto"><svg class="text-white group-hover:text-primary" fill="none" height="15" viewbox="0 0 24 15" width="24" xmlns="http://www.w3.org/2000/svg"> <path d="M2.82 0.159912L12 9.33991L21.18 0.159912L24 2.99991L12 14.9999L0 2.99991L2.82 0.159912Z" fill="currentColor" id="Vector"></path> </svg></div>
// </div>
// </div>

// <div class="border-b pb-4 border-lightColorBorder hover:border-b-primary  shadow-lg hover:shadow-2xl transition-all duration-300 overflow-hidden">
// <div class="group flex items-start py-4 transition duration-200">
// <div class="h-10 relative group-hover:h-full overflow-hidden transition-all duration-300">
// <p class="lg:text-3xl text-xl font-bold">What if a competition does not sell out?</p>

// <p class="text-base mt-4 text-grayColor">Your ticket number(s) will be shown as soon as your order is confirmed and will be available under <span class="text-primary font-bold">&lsquo;My Account&rsquo;</span> and in your email confirmation.</p>
// </div>

// <div class="text-3xl group-hover:rotate-180 transition duration-500 ml-auto"><svg class="text-white group-hover:text-primary" fill="none" height="15" viewbox="0 0 24 15" width="24" xmlns="http://www.w3.org/2000/svg"> <path d="M2.82 0.159912L12 9.33991L21.18 0.159912L24 2.99991L12 14.9999L0 2.99991L2.82 0.159912Z" fill="currentColor" id="Vector"></path> </svg></div>
// </div>
// </div>

// <div class="border-b pb-4 border-lightColorBorder hover:border-b-primary  shadow-lg hover:shadow-2xl transition-all duration-300 overflow-hidden">
// <div class="group flex items-start py-4 transition duration-200">
// <div class="h-10 relative group-hover:h-full overflow-hidden transition-all duration-300">
// <p class="lg:text-3xl text-xl font-bold">Is ‘Winnar’ a scam?</p>

// <p class="text-base mt-4 text-grayColor">Your ticket number(s) will be shown as soon as your order is confirmed and will be available under <span class="text-primary font-bold">&lsquo;My Account&rsquo;</span> and in your email confirmation.</p>
// </div>

// <div class="text-3xl group-hover:rotate-180 transition duration-500 ml-auto"><svg class="text-white group-hover:text-primary" fill="none" height="15" viewbox="0 0 24 15" width="24" xmlns="http://www.w3.org/2000/svg"> <path d="M2.82 0.159912L12 9.33991L21.18 0.159912L24 2.99991L12 14.9999L0 2.99991L2.82 0.159912Z" fill="currentColor" id="Vector"></path> </svg></div>
// </div>
// </div>

// <div class="border-b pb-4 border-lightColorBorder hover:border-b-primary  shadow-lg hover:shadow-2xl transition-all duration-300 overflow-hidden">
// <div class="group flex items-start py-4 transition duration-200">
// <div class="h-10 relative group-hover:h-full overflow-hidden transition-all duration-300">
// <p class="lg:text-3xl text-xl font-bold">Why am I receiving communications from Winnar?</p>

// <p class="text-base mt-4 text-grayColor">Your ticket number(s) will be shown as soon as your order is confirmed and will be available under <span class="text-primary font-bold">&lsquo;My Account&rsquo;</span> and in your email confirmation.</p>
// </div>

// <div class="text-3xl group-hover:rotate-180 transition duration-500 ml-auto"><svg class="text-white group-hover:text-primary" fill="none" height="15" viewbox="0 0 24 15" width="24" xmlns="http://www.w3.org/2000/svg"> <path d="M2.82 0.159912L12 9.33991L21.18 0.159912L24 2.99991L12 14.9999L0 2.99991L2.82 0.159912Z" fill="currentColor" id="Vector"></path> </svg></div>
// </div>
// </div>


// <div class="border-b pb-4 border-lightColorBorder hover:border-b-primary  shadow-lg hover:shadow-2xl transition-all duration-300 overflow-hidden">
// <div class="group flex items-start py-4 transition duration-200">
// <div class="h-10 relative group-hover:h-full overflow-hidden transition-all duration-300">
// <p class="lg:text-3xl text-xl font-bold">How do the instant win competitions work?</p>

// <p class="text-base mt-4 text-grayColor">Your ticket number(s) will be shown as soon as your order is confirmed and will be available under <span class="text-primary font-bold">&lsquo;My Account&rsquo;</span> and in your email confirmation.</p>
// </div>

// <div class="text-3xl group-hover:rotate-180 transition duration-500 ml-auto"><svg class="text-white group-hover:text-primary" fill="none" height="15" viewbox="0 0 24 15" width="24" xmlns="http://www.w3.org/2000/svg"> <path d="M2.82 0.159912L12 9.33991L21.18 0.159912L24 2.99991L12 14.9999L0 2.99991L2.82 0.159912Z" fill="currentColor" id="Vector"></path> </svg></div>
// </div>
// </div>

// <div class="border-b pb-4 border-lightColorBorder hover:border-b-primary  shadow-lg hover:shadow-2xl transition-all duration-300 overflow-hidden">
// <div class="group flex items-start py-4 transition duration-200">
// <div class="h-10 relative group-hover:h-full overflow-hidden transition-all duration-300">
// <p class="lg:text-3xl text-xl font-bold">How can I ensure that I continue to play this safely?</p>

// <p class="text-base mt-4 text-grayColor">Your ticket number(s) will be shown as soon as your order is confirmed and will be available under <span class="text-primary font-bold">&lsquo;My Account&rsquo;</span> and in your email confirmation.</p>
// </div>

// <div class="text-3xl group-hover:rotate-180 transition duration-500 ml-auto"><svg class="text-white group-hover:text-primary" fill="none" height="15" viewbox="0 0 24 15" width="24" xmlns="http://www.w3.org/2000/svg"> <path d="M2.82 0.159912L12 9.33991L21.18 0.159912L24 2.99991L12 14.9999L0 2.99991L2.82 0.159912Z" fill="currentColor" id="Vector"></path> </svg></div>
// </div>
// </div>

// <div class="border-b pb-4 border-lightColorBorder hover:border-b-primary  shadow-lg hover:shadow-2xl transition-all duration-300 overflow-hidden">
// <div class="group flex items-start py-4 transition duration-200">
// <div class="h-10 relative group-hover:h-full overflow-hidden transition-all duration-300">
// <p class="lg:text-3xl text-xl font-bold">I am receiving spam on email or via SMS.</p>

// <p class="text-base mt-4 text-grayColor">Your ticket number(s) will be shown as soon as your order is confirmed and will be available under <span class="text-primary font-bold">&lsquo;My Account&rsquo;</span> and in your email confirmation.</p>
// </div>

// <div class="text-3xl group-hover:rotate-180 transition duration-500 ml-auto"><svg class="text-white group-hover:text-primary" fill="none" height="15" viewbox="0 0 24 15" width="24" xmlns="http://www.w3.org/2000/svg"> <path d="M2.82 0.159912L12 9.33991L21.18 0.159912L24 2.99991L12 14.9999L0 2.99991L2.82 0.159912Z" fill="currentColor" id="Vector"></path> </svg></div>
// </div>
// </div>

// <div class="border-b pb-4 border-lightColorBorder hover:border-b-primary  shadow-lg hover:shadow-2xl transition-all duration-300 overflow-hidden">
// <div class="group flex items-start py-4 transition duration-200">
// <div class="h-10 relative group-hover:h-full overflow-hidden transition-all duration-300">
// <p class="lg:text-3xl text-xl font-bold">How do I opt out from some or all of the communications?</p>

// <p class="text-base mt-4 text-grayColor">Your ticket number(s) will be shown as soon as your order is confirmed and will be available under <span class="text-primary font-bold"><span class="text-primary font-bold">&lsquo;My Account&rsquo;</span></span> and in your email confirmation.</p>
// </div>

// <div class="text-3xl group-hover:rotate-180 transition duration-500 ml-auto"><svg class="text-white group-hover:text-primary" fill="none" height="15" viewbox="0 0 24 15" width="24" xmlns="http://www.w3.org/2000/svg"> <path d="M2.82 0.159912L12 9.33991L21.18 0.159912L24 2.99991L12 14.9999L0 2.99991L2.82 0.159912Z" fill="currentColor" id="Vector"></path> </svg></div>
// </div>
// </div>





// <div class="border-b pb-4 border-lightColorBorder hover:border-b-primary  shadow-lg hover:shadow-2xl transition-all duration-300 overflow-hidden">
// <div class="group flex items-start py-4 transition duration-200">
// <div class="h-10 relative group-hover:h-full overflow-hidden transition-all duration-300">
// <p class="lg:text-3xl text-xl font-bold">Is it animated?</p>

// <p class="text-base mt-4 text-grayColor">Your ticket number(s) will be shown as soon as your order is confirmed and will be available under <span class="text-primary font-bold">&lsquo;My Account&rsquo;</span> and in your email confirmation.</p>
// </div>

// <div class="text-3xl group-hover:rotate-180 transition duration-500 ml-auto"><svg class="text-white group-hover:text-primary" fill="none" height="15" viewbox="0 0 24 15" width="24" xmlns="http://www.w3.org/2000/svg"> <path d="M2.82 0.159912L12 9.33991L21.18 0.159912L24 2.99991L12 14.9999L0 2.99991L2.82 0.159912Z" fill="currentColor" id="Vector"></path> </svg></div>
// </div>
// </div>
// </div>
// </div>
// </div>
// </section>
// `;


const CmsFunc = dynamic(() => import('~/components/app/cms/index'), {
  ssr: true,
});

const findElementsWithAttribute = (node: any) => {
  if (node.type === 'tag') {
    const shortcode = node?.attribs;
    console.log(shortcode, 'shortcode');

    if (shortcode?.data === 'main-carousel') {
      console.log(node, 'main-carousel');
      const imageCrousel = JSON.parse(node?.children[0]?.data) as [];
      console.log(imageCrousel, 'imageCrousel');

      if (imageCrousel.length) {
        console.log('empty');
        console.log({ imageCrousel });
        return <AboutCarousel imageCrousel={imageCrousel} />;
      }
    } else {
      console.log(node, 'element node');
      return node;
    }
  }
};

export default function CmsPage({ storeBlogsData }: any) {
  console.log(storeBlogsData?.CMSDescription[0]?.content, 'storeBlogsData');
  const reactElements = parse(storeBlogsData?.CMSDescription[0]?.content || '', {
    replace: findElementsWithAttribute,
  });

  return (
    <div className=" w-full bg-bg-1 py-2">
      <div className="mx-auto max-w-[1600px]">{reactElements}</div>
    </div>
  );
}
