import AboutCarousel from './about_carousel';
import { Button } from '~/components/ui/button';
import NextImage from '~/components/ui/img';
import { useDispatch, useSelector } from 'react-redux';
import { RootState } from '~/store/store';
import langContent from '~/locales';
import arthur from '~/public/assets/founder-arthur.jpg';
import murray from '~/public/assets/founder-murray.jpg';
import ryan from '~/public/assets/founder-ryan.jpg';

export default function AboutUs() {

  const { lang } = useSelector((state: RootState) => state.layout);

  return (

    lang.lang_id === 1 ?
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
              </linearGradient>
              <linearGradient
                id="paint1_linear_190_685"
                x1="706"
                y1="-1.26162e-05"
                x2="220"
                y2="563"
                gradientUnits="userSpaceOnUse"
              >
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

      :
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
                سحوبات 
                </p>
                <p className="relative h-[35px] -top-[50%]   mt-3 text-white drop-shadow-2xl text-center w-full text-4xl  lg:text-5xl   uppercase font-[900]">
                سيارات فخمة
                </p>
              </div>
            </div>
          </div>
        </div>
        <div className="w-full z-50 -mt-24">
          <div className="bg-primary lg:w-[400px] sm:w-[400px]  px-8 py-8 sm:py-16 md:py-20 aboutus-card  mx-4 lg:ml-14 mb-8 sm:mb-14 md:mb-20">
            <h2 className="font-black text-2xl lg:text-5xl md:text-3xl  text-black">
            مهمتنا
            </h2>
            <p className="text-background pt-5 text-sm font-medium">
            شركة Winnar، هي شركة مسابقات للفوز بسيارات فاخرة التي تقدم لعشاق السيارات فرصة مثيرة للفوز بسيارة أحلامهم والعديد من الجوائز الأخرى.
            <br />
              <br />
             من خلال تجاربنا الجذابة ومبادراتنا الخيرية، نهدف إلى خلق لحظات لا تُنسى تتجاوز حماسة الفوز! انضم إلى Winnar.com اليوم واستعد للقيادة في سيارة أحلامك.
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
              </linearGradient>
              <linearGradient
                id="paint1_linear_190_685"
                x1="706"
                y1="-1.26162e-05"
                x2="220"
                y2="563"
                gradientUnits="userSpaceOnUse"
              >
              </linearGradient>
            </defs>
          </svg>
        </div>
        <AboutCarousel
          heading={`تعرف على قوتنا الدافعة`}
          pera={`الفريق المتحمس وراء Winnar`}
          imageCrousel={carouselDataAR}
        />
        {/* bg-Image class to add bg iin UVP section */}
        <div className="w-full py-10 lg:py-32 lg:px-14 md:px-14 px-4 z-40 text-center mobbg">
          <div className=" text-center flex flex-col lg:flex-row gap-x-20 justify-center items-start font-sans lg:mx-auto">
            <p className=" text-white drop-shadow-2xl text-center  font-black text-2xl  lg:text-4xl lg:w-fit sm:mb-5">
            قيمتنا الفريدة
            </p>
            <div className=" font-normal max-w-3xl  w-fit">
              <p className=" text-grayColor drop-shadow-2xl text-start text-xl  ">
              في شركة Winnar، نقدم للأفراد الذين يتمتعون بالذوق الرفيع فرصة حصرية للفوز بسيارة أحلامهم من مجموعتنا المنتقاة بعناية من العلامات التجارية المتميزة في الأداء والفخامة والرقي.
              </p>
              <p className=" text-grayColor drop-shadow-2xl text-start text-md   pt-10">
              ما يميزنا هو تركيزنا الثابت على الشفافية والعدالة والإثارة. على عكس طرق شراء السيارات التقليدية، يوفر موقع Winnar.com منصة مميزة حيث يمتلك كل مشارك القدرة على الحصول على سيارة أحلامه. بفضل عملية اختيارنا الشفافة، يمكن للمشاركين أن يكونوا واثقين من أن فرصهم عادلة، وإثارة الفوز لا مثيل لها. انضم إلينا اليوم لتجربة تقوق كل التطلعات وكن جزءاً من رحلة سباق استثنائية.
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
              احصل على سيارة أحلامك بجزء بسيط من السعر!
              </p>
              <p className="relative h-[35px] -top-[50%]   mt-6 text-white drop-shadow-2xl text-center w-full text-2xl  lg:text-4xl font-[900]">
              الفوز يوم الأحد الساعة 8 مساءً
              </p>
              <a href="/cars">
                <Button
                  className=" px-8 mt-6 py-2  bg-primary clip-style text-black font-sans font-[900] text-xl"
                  variant="default"
                >
                  شارك الآن
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
    img: arthur,
    heading: 'Arthur',
    text: 'Co-Founder',
    hoverdesc: 'Lorem Ipus is dummt text.',
  },
  {
    img: murray,
    heading: 'Murray',
    text: 'Co-Founder',
    hoverdesc: 'Lorem Ipus is dummt text.',
  },
  {
    img: ryan,
    heading: 'Ryan',
    text: 'Co-Founder',
    hoverdesc: 'Lorem Ipus is dummt text.',
  },
];


const carouselDataAR = [
  {
    img: arthur,
    heading: 'Arthur',
    text: 'Co-Founder',
    hoverdesc: 'Lorem Ipus is dummt text.',
  },
  {
    img: murray,
    heading: 'Murray',
    text: 'Co-Founder',
    hoverdesc: 'Lorem Ipus is dummt text.',
  },
  {
    img: ryan,
    heading: 'Ryan',
    text: 'Co-Founder',
    hoverdesc: 'Lorem Ipus is dummt text.',
  },
];