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
          '<div class="flex flex-row md:flex-col md:justify-start md:items-start space-x-2 md:space-y-4"><div class="flex space-x-1 md:space-x-4 flex-1 md:flex-none"><p class="timer-box flex flex-col gap-1 md:gap-3 items-center py-3 px-1 md:p-4 min-w-fit w-full border-2 border-primary rounded-xl"><span class="text-3xl sm:text-5xl font-normal text-primary">' +
          days +
          '</span><span class="text-xs sm:text-base text-white">' +
          'Days' +
          '</span></p> ' +
          '<p class="timer-box flex flex-col gap-1 md:gap-3 items-center py-3 px-1 md:p-4 min-w-fit w-full border-2 border-primary rounded-xl"><span class="text-3xl sm:text-5xl font-normal text-primary">' +
          hours +
          '</span><span class="text-xs sm:text-base text-white">' +
          'Hours' +
          '</span></p> ' +
          '<p class="timer-box flex flex-col gap-1 md:gap-3 items-center py-3 px-1 md:p-4 min-w-fit w-full border-2 border-primary rounded-xl"><span class="text-3xl sm:text-5xl font-normal text-primary">' +
          minutes +
          '</span><span class="text-xs sm:text-base text-white">' +
          'Mins' +
          '</span></p> ' +
          '<p class="timer-box flex flex-col gap-1 md:gap-3 items-center py-3 px-1 md:p-4 min-w-fit w-full border-2 border-primary rounded-xl"><span class="text-3xl sm:text-5xl font-normal text-primary">' +
          seconds +
          '</span><span class="text-xs sm:text-base text-white">' +
          'Secs' +
          '</span></p></div></div>';
      }
    }, 1000);
  };

  return (
        <div className="w-full relative counterbx mt-6">
        <div className="flex flex-col md:flex-row justify-start md:row md:items-start text-lg font-bold md:text-2xl mb-2 text-white">Competition closes in</div>
        <div ref={counterRef}></div> 
        </div>
  );
};

export default CountDown;
