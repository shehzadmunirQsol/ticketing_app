import AboutCarousel from './about_carousel';
import { Button } from '~/components/ui/button';
import NextImage from '~/components/ui/img';
import { useDispatch, useSelector } from 'react-redux';
import { RootState } from '~/store/store';

export default function AboutUs() {

  const { lang } = useSelector((state: RootState) => state.layout);

  return (
    <div className="bg-background">
      <div className="w-full relative !h-[450px] md:!h-[550px] text-center">
        <div className="relative w-full h-full bg-black/50">
          <NextImage
            src="https://media.winnar.com/upload/8d6ed34e-7362-4e98-8731-8f9d03c4bec7-WINNAR-04-small.png"
            alt="/"
            fill
            quality={100}
            className=" object-cover  w-full h-full opacity-90"
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
        <div className="bg-primary lg:w-[400px] sm:w-[400px]  px-8 py-8 sm:py-16 md:py-20 aboutus-card  mx-4 lg:ml-14 mb-8 sm:mb-14 md:mb-20">
          <h2 className="font-black text-2xl lg:text-5xl md:text-3xl  text-black">
            Our Mission
          </h2>
          <p className="text-background pt-5 text-sm font-medium">
            Winnar, the prestige car raffle company that offers car enthusiasts
            the thrilling opportunity to win their coveted dream car and many
            other prizes.
            <br />
            <br />
            Through our captivating experiences and philanthropic initiatives,
            we aim to create memorable moments that extend beyond the thrill of
            winning! Join Winnar.com today and get ready to drive away in the
            car of your dreams.
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
      <AboutCarousel
        heading={`Meet Our Driving Force`}
        pera={`The Passionate Team Behind Winnar`}
        imageCrousel={carouselData}
      />
      {/* bg-Image class to add bg iin UVP section */}
      <div className="w-full py-10 lg:py-32 lg:px-14 md:px-14 px-4 z-40 text-center mobbg">
        <div className=" text-center flex flex-col lg:flex-row gap-x-20 justify-center items-start font-sans lg:mx-auto">
          <p className=" text-white drop-shadow-2xl text-center  font-black text-2xl  lg:text-4xl lg:w-fit sm:mb-5">
            OUR UVP
          </p>
          <div className=" font-normal max-w-3xl  w-fit">
            <p className=" text-grayColor drop-shadow-2xl text-start text-xl  ">
              At Winnar, we offer discerning individuals an exclusive
              opportunity to win their coveted dream car from our meticulously
              curated collection of performance, luxury, and prestige brands.
            </p>
            <p className=" text-grayColor drop-shadow-2xl text-start text-md   pt-10">
              What sets us apart is our unwavering focus on transparency,
              fairness, and excitement. Unlike traditional car purchasing
              methods, Winnar.com provides an electrifying platform where every
              participant is empowered to drive away in the car of their dreams.
              With our transparent selection process, participants can be
              confident that their chances are fair, and the thrill of winning
              is unparalleled. Join us today to experience the epitome of
              automotive aspirations and be a part of an exceptional race
              journey.
            </p>
          </div>
        </div>
      </div>

      <div className="w-full relative h-[250px] lg:h-[550px] z-40 text-center">
        <NextImage
          src="https://media.winnar.com/upload/about-background-2.png"
          alt="/"
          fill
          quality={100}
          className=" object-cover w-full  h-full block bg-black/50"
        />
        <div className="absolute h-[35px] w-full text-center top-[50%] flex items-center">
          <div className="w-full text-center px-4">
            <p className=" text-white drop-shadow-2xl text-center w-full text-xl lg:text-3xl">
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
    </div>
  );
}

const carouselData = [
  {
    img: 'https://media.winnar.com/upload/founder.png',
    heading: 'Scott L. Hughes',
    text: 'Co-Founder',
    hoverhead: 'Scott L. Hughes',
    hoverpera: 'Co-Founder',
    hoverdesc:
      'Aenean vulputate eleifend tellus Aenean leo ligula porttitor eu consequat vitae eleifend ac enim. Aliquam lorem ante dapibus in viverra quis feugiat a tellus Phasellus viverra nulla ut metus varius laoreet Quisque rutrum',
  },
  {
    img: 'https://media.winnar.com/upload/founder-1.png',
    heading: 'Scott L. Hughes',
    text: 'Founder',
    hoverhead: 'Scott L. Hughes',
    hoverpera: 'Founder',
    hoverdesc:
      'Aenean vulputate eleifend tellus Aenean leo ligula porttitor eu consequat vitae eleifend ac enim. Aliquam lorem ante dapibus in viverra quis feugiat a tellus Phasellus viverra nulla ut metus varius laoreet Quisque rutrum',
  },
  {
    img: 'https://media.winnar.com/upload/founder-2.png',
    heading: 'Eric M. Carroll',
    text: 'Founder',
    hoverhead: 'Scott L. Hughes',
    hoverpera: 'Founder',
    hoverdesc:
      'Aenean vulputate eleifend tellus Aenean leo ligula porttitor eu consequat vitae eleifend ac enim. Aliquam lorem ante dapibus in viverra quis feugiat a tellus Phasellus viverra nulla ut metus varius laoreet Quisque rutrum',
  },
  {
    img: 'https://media.winnar.com/upload/founder-3.png',
    heading: 'Ronnie D. Blake',
    text: 'Founder',
    hoverhead: 'Scott L. Hughes',
    hoverpera: 'Founder',
    hoverdesc:
      'Aenean vulputate eleifend tellus Aenean leo ligula porttitor eu consequat vitae eleifend ac enim. Aliquam lorem ante dapibus in viverra quis feugiat a tellus Phasellus viverra nulla ut metus varius laoreet Quisque rutrum',
  },
  {
    img: 'https://media.winnar.com/upload/founder.png',
    heading: 'Marvin L. Orr',
    text: 'Founder',
    hoverhead: 'Scott L. Hughes',
    hoverpera: 'Founder',
    hoverdesc:
      'Aenean vulputate eleifend tellus Aenean leo ligula porttitor eu consequat vitae eleifend ac enim. Aliquam lorem ante dapibus in viverra quis feugiat a tellus Phasellus viverra nulla ut metus varius laoreet Quisque rutrum',
  },
  {
    img: 'https://media.winnar.com/upload/founder.png',
    heading: 'Ronnie D. Blake',
    text: 'Founder',
    hoverhead: 'Scott L. Hughes',
    hoverpera: 'Founder',
    hoverdesc:
      'Aenean vulputate eleifend tellus Aenean leo ligula porttitor eu consequat vitae eleifend ac enim. Aliquam lorem ante dapibus in viverra quis feugiat a tellus Phasellus viverra nulla ut metus varius laoreet Quisque rutrum',
  },
];
