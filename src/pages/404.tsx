import Link from 'next/link';

const Custom404 = () => {
  return (
    <>
      <div className="relative pt-24"></div>
      <div className="flex flex-col items-center justify-center min-h-[65vh]">
        <h1 className="mb-4 text-6xl font-semibold text-primary">404</h1>
        <p className="mb-4 text-lg text-gray-600">Oops Looks like youre lost</p>
        <div className="animate-bounce">
          <svg
            className="mx-auto h-16 w-16 text-primary"
            fill="none"
            viewBox="0 0 24 24"
            stroke="currentColor"
          >
            <path
              strokeLinecap="round"
              strokeLinejoin="round"
              strokeWidth="2"
              d="M12 19l9 2-9-18-9 18 9-2zm0 0v-8"
            ></path>
          </svg>
        </div>
        <p className="mt-4 mb-6 text-gray-600">Lets get you back</p>
        <Link href={'/'}>
          <button className="buttonClass font-sans">
            <span className="button_lg">
              <span className="button_sl"></span>
              <p className="button_text font-sans">Go to Home</p>
            </span>
          </button>
        </Link>
      </div>
    </>
  );
};

export default Custom404;
