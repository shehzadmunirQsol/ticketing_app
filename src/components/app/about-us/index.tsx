import React, { useState } from 'react';
import BannerTitle from '~/components/common/banner_title';
import BackgroundImage from '~/public/assets/account.png';
import AccountView from '~/components/app/account/account-view';
import { trpc } from '~/utils/trpc';
import { useToast } from '~/components/ui/use-toast';
import { useRouter } from 'next/router';

const AboutUs = () => {
  const { toast } = useToast();
  const router = useRouter();

  const [counter, setCounter] = useState(0);

  const { data: cms, isLoading } = trpc.cms.getCmsAboutUs.useQuery(
    {},
    {
      refetchOnWindowFocus: false,
    },
  );
  console.log(cms, 'customer');


    const data = `<div>
    <style>
        @font-face {
            font-family: "Proxima Nova Extrabold";
            src: url("https://db.onlinewebfonts.com/t/5243e362912f7119ede836ab03f23ac7.eot");
            src: url("https://db.onlinewebfonts.com/t/5243e362912f7119ede836ab03f23ac7.eot?#iefix") format("embedded-opentype"),
                url("https://db.onlinewebfonts.com/t/5243e362912f7119ede836ab03f23ac7.woff2") format("woff2"),
                url("https://db.onlinewebfonts.com/t/5243e362912f7119ede836ab03f23ac7.woff") format("woff"),
                url("https://db.onlinewebfonts.com/t/5243e362912f7119ede836ab03f23ac7.ttf") format("truetype"),
                url("https://db.onlinewebfonts.com/t/5243e362912f7119ede836ab03f23ac7.svg#Proxima Nova Extrabold") format("svg");
        }

        @font-face {
            font-family: "Proxima Nova Regular";
            src: url("https://db.onlinewebfonts.com/t/0376a58122a881d16a294512d3c947b1.eot");
            src: url("https://db.onlinewebfonts.com/t/0376a58122a881d16a294512d3c947b1.eot?#iefix") format("embedded-opentype"),
                url("https://db.onlinewebfonts.com/t/0376a58122a881d16a294512d3c947b1.woff2") format("woff2"),
                url("https://db.onlinewebfonts.com/t/0376a58122a881d16a294512d3c947b1.woff") format("woff"),
                url("https://db.onlinewebfonts.com/t/0376a58122a881d16a294512d3c947b1.ttf") format("truetype"),
                url("https://db.onlinewebfonts.com/t/0376a58122a881d16a294512d3c947b1.svg#Proxima Nova Regular") format("svg");
        }



        .container {
            position: relative;
            text-align: center;
            z-index: 20;
        }

        .about-image {
            width: 100%;
            height: 100%;
            max-height: 25rem;
            object-fit: cover;

        }

        .title {
            color: #eaeaea;
            text-align: center;
            font-size: 32px;
            font-weight: 900 !important;

            text-transform: uppercase;
            z-index: 2;

        }

        .title-1 {
            color: #eaeaea;
            text-align: center;
            font-family: Proxima Nova Regular;

            font-size: 32px;
            width: 100%;
            z-index: 2;

        }

        .centered {
            position: absolute;
            top: 50%;
            left: 50%;
            transform: translate(-50%, -50%);
        }

        .centered-1 {
            position: absolute;
            top: 30%;
            left: 50%;
            transform: translate(-50%, -50%);
        }


        .container-fixed {
            position: block;
        }

        .mission {
            position: absolute;
            top: 11rem;
            left: 2rem;
            text-align: left;
        }

        .mission .box {
            background-color: #23d2b3;

            padding: 40px;
            max-width: 320px;

            clip-path: polygon(0 0,
                    calc(100% - 60px) 0,
                    100% 60px,
                    100% 100%,
                    0 100%);
        }

        .box-title {
            color: #060a0d;
            font-family: Proxima Nova Extrabold;

            font-size: 36px;
            font-style: normal;
            -webkit-font-smoothing: antialiased;
            font-weight: 900 !important;
            line-height: 30%;
            letter-spacing: -2px;
            text-transform: uppercase;
        }

        .box-para {
            color: #060a0d;
            font-family: Proxima Nova Regular;

            font-size: 16px;
            font-style: normal;
            font-weight: 800 !important;
            line-height: 130%;
        }

        .glow {
            -webkit-box-shadow: 0px 0px 116px 48px rgba(45, 255, 196, 0.28);
            -moz-box-shadow: 0px 0px 116px 48px rgba(45, 255, 196, 0.28);
            box-shadow: 0px 0px 116px 48px rgba(45, 255, 196, 0.28);
        }

        .space-make {
            height: 60vh;
        }

        .button {
            background-color: #23d2b3;
            clip-path: polygon(0% 0%,
                    8.13% 37.12%,
                    0% 22.7%,
                    0% 100%,
                    100% 98.75%,
                    100% 42.82%,
                    100% 0%);

            padding: 10px 20px;

            font-family: Proxima Nova Extrabold;
        }


        .main-flex {
            display: flex;
            justify-content: center;
            align-items: start;
            position: relative;
            column-gap: 3rem;
            top: 0%;

        }

        .para-text {
            font-size: 36px;
            font-weight: 900;

        }


        #main-flex-back {
            position: absolute; 
            top: 4rem;
            right: 4rem;
            left: 4rem;
            bottom: 4rem;
        }

        @media screen and (max-width: 614px) {
            .about-image {
                height: 20rem;
            }

            .mission {
                top: 16rem;
                margin: 5px;
                left: 0;
            }

            .mission .box {
                padding: 30px;
                margin: 30px;
                max-width: 320px;
            }

            .box-title {
                font-size: 32px;
            }
        }
    </style>
    <div class="container">
        <img src="https://media.winnar.com/upload/about-page-background.png" class="about-image" alt="/" />

        <p class="title centered">PRESTIGE CAR RAFFLES</p>

        <div class="mission">
            <div class="box">
                <p class="box-title">OUR MISSION</p>
                <p class="box-para">
                    Winnar, the prestige car ra e company that o ers car enthusiasts the
                    thrilling opportunity to win their coveted dream car and many other
                    prizes. Through our captivating experiences and philanthropic
                    initiatives, we aim to create memorable moments that extend beyond
                    the thrill of winning! Join Winnar.com today and get ready to drive
                    away in the car of your dreams.
                </p>
            </div>
            <div class="glow"></div>
        </div>
      
    </div>

    <div class="container container-fixed space-make" style="color: #eaeaea"></div>

    <div class="container container-fixed"
        style="display:flex; justify-content: space-between; color:#eaeaea; margin:0px 2rem;">
        <p style="font-family: Proxima Nova Extrabold; font-size: 36px;">Meet Our Team</p>
        <p>arrow buttoons</p>
    </div>

    <div style="color:#eaeaea; ">
        <div class="container container-fixed background-mvp">
            <img src="https://media.winnar.com/upload/about-background-1.png" class="about-image" alt="/" />
            <div>
                <div class="main-flex" id="main-flex-back" >
                    <p class="para-text" style="text-align: right; width:100%; max-width:12rem; margin-top: 0rem;">
                        OUR MVP
                    </p>
                    

                    <div style="
                  font-family: Proxima Nova Regular;
                  font-size:18px;
                  text-align: left;
                  max-width: 35rem;
                ">
                        <div>
                            At Winnar, we offer discerning individuals an exclusive
                            opportunity to win their coveted dream car from our meticulously
                            curated collection of performance, luxury, and prestige brands.
                        </div>
                        <div class="" style="padding-top: 1rem;">
                            What sets us apart is our unwavering focus on transparency,
                            fairness, and excitement. Unlike traditional car purchasing
                            methods, Winnar.com provides an electrifying platform where every
                            participant is empowered to drive away in the car of their dreams.
                            With our transparent selection process, participants can be
                            confident that their chances are fair, and the thrill of winning
                            is unparalleled. Join us today to experience the epitome of
                            automotive aspirations and be a part of an exceptional ra e
                            journey.
                        </div>
                    </div>
                </div>
            </div>
        </div>

    </div>


    <div>
        <div class="container container-fixed">
            <img src="https://media.winnar.com/upload/about-background-2.png" class="about-image" style="height: 50vh"
                alt="/" />

            <p class="title-1 centered-1" style="padding: 0 5px; top: 5rem; font-weight: thin">
                Get your dream car at a fraction of the price!
            </p>
            <p class="title centered" style="font-size: 28px; top: 10rem; width: 100%">
                WIN SUNDAY 8 PM
            </p>

            <button class="button centered" style="top: 15rem">
                <a href="/cars" style="
              text-decoration: none;
              color: #060a0d;
              font-weight: 900;
              letter-spacing: -0.4px;
            ">
                    ENTER NOW
                </a>
            </button>
        </div>
    </div>
    </div>
`;


  return (
    <div className='mt-96'>
      {/* {cms?.map((item:any, i) => {
        console.log(item,"items found")
        return (
          // <div key={i} dangerouslySetInnerHTML={{ __html: item.CMSDescription[0]?.content }}>
        );
      })} */}
          <div  dangerouslySetInnerHTML={{ __html: data }}>
          </div>
    </div>
  );
};

export default AboutUs;
