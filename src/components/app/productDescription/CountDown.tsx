import React, { useEffect } from 'react';
import { useSelector } from 'react-redux';
import { RootState } from '~/store/store';
import langContent from '~/locales';

const CountDown = ({ dateString }: any) => {

  const { lang } = useSelector((state: RootState) => state.layout);
  const counterRef = React.useRef<any>(null);

  useEffect(() => {
    counter();
  }, []);
  /**Counter */

  const counter = () => {
    // Set the date we're counting down to
    const countDownDate: any = dateString; //new Date("Oct 31, 2023 24:00:00").getTime();

    // Update the count down every 1 second
    setInterval(() => {
      // Get todays date and time
      const now: any = new Date().getTime();

      // Find the distance between now an the count down date
      const distance: any = countDownDate - now;

      // Time calculations for days, hours, minutes and seconds
      const days: any = Math.floor(distance / (1000 * 60 * 60 * 24));
      const hours: any = Math.floor(
        (distance % (1000 * 60 * 60 * 24)) / (1000 * 60 * 60),
      );
      const minutes: any = Math.floor(
        (distance % (1000 * 60 * 60)) / (1000 * 60),
      );
      const seconds: any = Math.floor((distance % (1000 * 60)) / 1000);

      // Output the result in an element 
      if (counterRef && counterRef.current!==null) {
        counterRef.current.innerHTML =
          '<div class="counterrow"><p class="timer-box">' +
          days +
          '<span>' +
          langContent[lang.lang].ProductDetail.countdown.days +
          '</span></p> ' +
          '<p class="timer-box">' +
          hours +
          '<span>' +
          langContent[lang.lang].ProductDetail.countdown.hours +
          '</span></p> ' +
          '<p class="timer-box">' +
          minutes +
          '<span>' +
          langContent[lang.lang].ProductDetail.countdown.mins +
          '</span></p> ' +
          '<p class="timer-box">' +
          seconds +
          '<span>' +
          langContent[lang.lang].ProductDetail.countdown.secs +
          '</span></p></div>';
      }
    }, 1000);
  };

  return (
        <div className="w-full relative counterbx mt-5">
        <div className="counttitle text-basic md:text-lg mb-2 text-white">{langContent[lang.lang].ProductDetail.countdown.title}</div>
        <div ref={counterRef}></div> 
        </div>
  );
};

export default CountDown;
