import Image from 'next/image';
import React from 'react';
import ImageAbout from '../../public/assets/abouthead.svg';
import { Button } from '../ui/button';
import { trpc } from '~/utils/trpc';

const About = () => {
  const { data: cms, isLoading }: any = trpc.cms.getCmsContent.useQuery(
    {},
    {
      refetchOnWindowFocus: false,
    },
  );

  console.log({ cms })
  return (
    // <div dangerouslySetInnerHTML={{__html: cms[2].CMSDescription[0].content ?? <>Html Content Not Found</>}}>

    // </div>
    // <div>
    //   {/* complete */}
    //   <div class="w-full relative h-[250px] lg:h-[550px] z-40 text-center">
    //     <Image
    //       src="https://media.winnar.com/upload/about-page-background.png"
    //       alt="/"
    //       fill
    //       quality={100}
    //       class=" object-cover block bg-black/50"
    //     />
    //     <div class="absolute h-[35px] w-full text-center top-[50%] flex items-center">
    //       <div class="w-full text-center ">
    //         <p class=" text-white drop-shadow-2xl text-center w-full text-4xl  lg:text-5xl tracking-tighter  uppercase font-[900]">
    //           PRESTIGE CAR
    //         </p>
    //         <p class="relative h-[35px] -top-[50%]   mt-6 text-white drop-shadow-2xl text-center w-full text-4xl  lg:text-5xl tracking-tighter  uppercase font-[900]">
    //           RAFFLES
    //         </p>
    //       </div>
    //     </div>
    //   </div>

    //   {/* complete */}
    //   <div>
    //     <div class="bg-primary w-[400px] px-8 py-20 aboutus-card ml-20">
    //       <p class="font-black text-5xl text-black">Our Mission</p>
    //       <p class="text-background pt-5 text-sm">
    //         Winnar, the prestige car ra e company that o ers car enthusiasts the
    //         thrilling opportunity to win their coveted dream car and many other
    //         prizes. Through our captivating experiences and philanthropic
    //         initiatives, we aim to create memorable moments that extend beyond
    //         the thrill of winning! Join Winnar.com today and get ready to drive
    //         away in the car of your dreams.
    //       </p>
    //     </div>
    //   </div>

    //   {/* complete */}
    //   <div class="absolute bottom-0 right-10">
    //     <svg
    //       width="474"
    //       height="895"
    //       viewBox="0 0 674 1095"
    //       fill="none"
    //       xmlns="http://www.w3.org/2000/svg"
    //     >
    //       <path
    //         d="M1.11379 547.5L486.887 1093.73H819.886L334.113 547.5H1.11379Z"
    //         stroke="url(#paint0_linear_190_685)"
    //       />
    //       <path
    //         d="M1.11379 546.728L486.887 0.5H819.886L334.113 546.728H1.11379Z"
    //         stroke="url(#paint1_linear_190_685)"
    //       />
    //       <defs>
    //         <linearGradient
    //           id="paint0_linear_190_685"
    //           x1="706"
    //           y1="1094.23"
    //           x2="220"
    //           y2="531.228"
    //           gradientUnits="userSpaceOnUse"
    //         >
    //           <stop stop-opacity="0" />
    //           <stop offset="1" stop-color="#454545" />
    //         </linearGradient>
    //         <linearGradient
    //           id="paint1_linear_190_685"
    //           x1="706"
    //           y1="-1.26162e-05"
    //           x2="220"
    //           y2="563"
    //           gradientUnits="userSpaceOnUse"
    //         >
    //           <stop stop-opacity="0" />
    //           <stop offset="1" stop-color="#454545" />
    //         </linearGradient>
    //       </defs>
    //     </svg>
    //   </div>

    //   {/* complete */}
    //   <div class="flex flex-row justify-between">
    //     <div>
    //       <p class="font-black text-white text-4xl uppercase">
    //         Meet Our Driving Force
    //       </p>
    //       <p class=" text-white text-2xl">
    //         The Passionate Team Behind Winnar
    //       </p>
    //     </div>
    //     <div class="flex flex-row gap-4">
    //       <div class="rounded-full p-4 bg-primary hover:bg-background ">
    //         <svg
    //           width="40"
    //           height="40"
    //           viewBox="0 0 20 20"
    //           fill="none"
    //           xmlns="http://www.w3.org/2000/svg"
    //         >
    //           <path
    //             class="slider_text"
    //             d="M12.8501 13.825L9.0251 10L12.8501 6.175L11.6668 5L6.66676 10L11.6668 15L12.8501 13.825Z"
    //             fill="#111111"
    //           />
    //         </svg>
    //       </div>
    //       <div class="rounded-full p-4 bg-primary">
    //         <svg
    //           width="40"
    //           height="40"
    //           viewBox="0 0 20 20"
    //           fill="none"
    //           xmlns="http://www.w3.org/2000/svg"
    //         >
    //           <path
    //             d="M7.1499 13.825L10.9749 10L7.1499 6.175L8.33324 5L13.3332 10L8.33324 15L7.1499 13.825Z"
    //             fill="#111111"
    //             class="slider_text"
    //           />
    //         </svg>
    //       </div>
    //     </div>
    //   </div>


    //   {/* complete */}

    //   <div class="w-full relative h-[250px] lg:h-[450px] z-40 text-center">
    //     <Image
    //       src="https://media.winnar.com/upload/about-background-1.png"
    //       alt="/"
    //       fill
    //       quality={100}
    //       class=" object-cover block bg-black/50"
    //     />

    //     <div class="absolute top-20 left-72">
    //       <div class=" text-center flex flex-row gap-x-4 justify-center items-start font-sans mx-auto">
    //         <p class=" text-white drop-shadow-2xl text-center  font-black text-2xl  lg:text-3xl w-fit">
    //           OUR MVP
    //         </p>
    //         <div class=" font-normal max-w-3xl w-fit">
    //           <p class=" text-white drop-shadow-2xl text-justify  ">
    //             At Winnar, we offer discerning individuals an exclusive
    //             opportunity to win their coveted dream car from our meticulously
    //             curated collection of performance, luxury, and prestige brands.
    //           </p>
    //           <p class=" text-white drop-shadow-2xl text-xs text-justify  pt-10">
    //             What sets us apart is our unwavering focus on transparency,
    //             fairness, and excitement. Unlike traditional car purchasing
    //             methods, Winnar.com provides an electrifying platform where
    //             every participant is empowered to drive away in the car of their
    //             dreams. With our transparent selection process, participants can
    //             be confident that their chances are fair, and the thrill of
    //             winning is unparalleled. Join us today to experience the epitome
    //             of automotive aspirations and be a part of an exceptional ra e
    //             journey.
    //           </p>
    //         </div>
    //       </div>
    //     </div>
    //   </div>

    //   {/* complete */}
    //   <div class="w-full relative h-[250px] lg:h-[550px] z-40 text-center">
    //     <Image
    //       src="https://media.winnar.com/upload/about-background-2.png"
    //       alt="/"
    //       fill
    //       quality={100}
    //       class=" object-cover block bg-black/50"
    //     />
    //     <div class="absolute h-[35px] w-full text-center top-[50%] flex items-center">
    //       <div class="w-full text-center ">
    //         <p class=" text-white drop-shadow-2xl text-center w-full text-2xl  lg:text-3xl">
    //           Get your dream car at a fraction of the price!
    //         </p>
    //         <p class="relative h-[35px] -top-[50%]   mt-6 text-white drop-shadow-2xl text-center w-full text-3xl  lg:text-4xl font-[900]">
    //           WIN SUNDAY 8 PM
    //         </p>
    //         <Button
    //           class=" px-16  bg-primary clip-style text-black font-sans font-[900] mt-5   text-xl"
    //           variant="default"
    //         >
    //           Enter Now
    //         </Button>
    //       </div>
    //     </div>
    //   </div>
    // </div>
    <>
    </>
  );
};

export default About;
