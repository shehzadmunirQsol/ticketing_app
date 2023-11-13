import React, { useEffect } from 'react';

const CountDown = ({ dateString }: any) => {

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
          'Days' +
          '</span></p> ' +
          '<p class="timer-box">' +
          hours +
          '<span>' +
          'Hours' +
          '</span></p> ' +
          '<p class="timer-box">' +
          minutes +
          '<span>' +
          'Mins' +
          '</span></p> ' +
          '<p class="timer-box">' +
          seconds +
          '<span>' +
          'Secs' +
          '</span></p></div>';
      }
    }, 1000);
  };

  return (
        <div className="w-full relative counterbx mt-5">
        <div className="text-basic md:text-lg mb-2 text-white">Competition closes in</div>
        <div ref={counterRef}></div> 
        </div>
  );
};

export default CountDown;
