// import { BiSolidChevronDown } from "react-s/bi";
import Image from 'next/image';

const Faqs = () => {
  return (
    <section>
      <div className="w-full relative h-[250px] lg:h-[550px]  text-center">
        <Image
          src="https://media.winnar.com/upload/about-page-background.png"
          alt="/"
          fill
          quality={100}
          className=" object-cover block bg-black/50"
        />
        <div className="absolute h-[35px] w-full text-center top-[50%] flex items-center">
          <div className="w-full text-center">
            <p className=" text-white drop-shadow-2xl text-center w-full text-4xl  lg:text-5xl tracking-tighter  uppercase font-[900]">
              FAQS
            </p>
          </div>
        </div>
      </div>

      <div className="px-4 md:px-14 relative  min-h-screen w-full flex flex-col gap-8 ">
        <div>
          <p className="lg:text-5xl md:text-4xl text-3xl text-start  font-black uppercase">
            Frequently asked questions
          </p>
          <div className="border-b-4 w-16 border-primary mt-4"></div>

          <div className="relative py-8  min-h-screen w-full flex flex-col  gap-8 ">
            <div className="border-b hover:border-green-400 shadow-lg hover:shadow-2xl transition-all duration-300 overflow-hidden">
              <div className="group flex items-start py-4 transition duration-200">
                <div className="h-10 relative group-hover:h-full overflow-hidden transition-all duration-300">
                  <p className="lg:text-3xl text-xl font-bold">Is it animated?</p>
                  <p className="text-base mt-4 text-grayColor">
                    Your ticket number(s) will be shown as soon as your order is
                    confirmed and will be available under ‘My Account’ and in
                    your email confirmation.
                  </p>
                </div>
                <div className="text-3xl group-hover:rotate-180 transition duration-500 ml-auto">
                  <svg
                    width="24"
                    height="15"
                    viewBox="0 0 24 15"
                    fill="none"
                    xmlns="http://www.w3.org/2000/svg"
                    className="text-white group-hover:text-green-400"
                  >
                    <path
                      id="Vector"
                      d="M2.82 0.159912L12 9.33991L21.18 0.159912L24 2.99991L12 14.9999L0 2.99991L2.82 0.159912Z"
                      fill="currentColor"
                    />
                  </svg>
                </div>
              </div>
            </div>

            <div className="border-b hover:border-green-400 shadow-lg hover:shadow-2xl transition-all duration-300 overflow-hidden">
              <div className="group flex items-start py-4 transition duration-200">
                <div className="h-10 relative group-hover:h-full overflow-hidden transition-all duration-300">
                  <p className="lg:text-3xl text-xl font-bold">Is it animated?</p>
                  <p className="text-base mt-4 text-grayColor">
                    Your ticket number(s) will be shown as soon as your order is
                    confirmed and will be available under ‘My Account’ and in
                    your email confirmation.
                  </p>
                </div>
                <div className="text-3xl group-hover:rotate-180 transition duration-500 ml-auto">
                  <svg
                    width="24"
                    height="15"
                    viewBox="0 0 24 15"
                    fill="none"
                    xmlns="http://www.w3.org/2000/svg"
                    className="text-white group-hover:text-green-400"
                  >
                    <path
                      id="Vector"
                      d="M2.82 0.159912L12 9.33991L21.18 0.159912L24 2.99991L12 14.9999L0 2.99991L2.82 0.159912Z"
                      fill="currentColor"
                    />
                  </svg>
                </div>
              </div>
            </div>
          </div>
        </div>
      </div>
    </section>
  );
};

export default Faqs;
