import { About } from '~/components/app/About';
import { Cta } from '~/components/app/Cta';
import { Features } from '~/components/app/Features';
import { Hero } from '~/components/app/Hero';
import { HowItWorks } from '~/components/app/HowItWorks';
import { Navbar } from '~/components/app/Navbar';
import { Newsletter } from '~/components/app/Newsletter';
import { Pricing } from '~/components/app/Pricing';
import { ScrollToTop } from '~/components/app/ScrollToTop';
import { Services } from '~/components/app/Services';
import { Sponsors } from '~/components/app/Sponsors';
import { Team } from '~/components/app/Team';
import { Testimonials } from '~/components/app/Testimonials';
import { NextPageWithLayout } from '~/pages/_app';

const IndexPage: NextPageWithLayout = () => (
  <>
    <div className="relative w-full !mx-auto">
      <Navbar />
      <Hero />
      <Sponsors />
      <About />
      <HowItWorks />
      <Features />
      <Services />
      <Cta />
      <Testimonials />
      <Team />
      <Pricing />
      <Newsletter />
    </div>
    {/* <Sponsors />
    <About />
    <HowItWorks />
    <Features /> */}
    {/* <Services />
    <Cta />
    <Testimonials />
    <Team />
    <Pricing />
    <Newsletter />
    <FAQ />
    <Footer /> */}
    <ScrollToTop />
  </>
);

export default IndexPage;
