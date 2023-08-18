import React, { useRef } from 'react';
import ProductSection from './product_section';
import CategorySection from './product_category';
import HowtoSection from './how_to_play';
import WhyChoose from './why_choose';
import Testimonials from './testimonials';
import BannerSlider from './banner_slider';
import { trpc } from '~/utils/trpc';
import { Button } from '@/ui/button';
function Home() {
  const registration = trpc.admin.register.useMutation({
    onSuccess: (res: any) => {
      console.log("return data", res);
    },
    onError(error) {
      console.log( error.message,"ERROR" );
    },
  })

  async function register(){
    console.log("Working")
    const values={
      name:"umair",
      email:"umair.qsols@gmail.com",
      role_id:1,
      password:"umair12345"
    }
    console.log("values: ",values)
    try {
      const response = await registration.mutateAsync({ ...values });  
      console.log("Response : ",response)
    } catch (error : any ){
      console.log("Error ",error)
      // const errorMessage = formatTrpcError(error?.shape?.message);
      
    }
  }
  

  return (
    <div className=" flex flex-col gap-8 min-h-screen w-screen  ">
      <div className="relative top-0">
        <BannerSlider />
      </div>
      {/* product section 1 */}
      <div className="relative flex flex-col gap-8 min-h-screen w-screen px-6 py-2 ">
        <ProductSection
          class="max-w-sm lg:max-w-xs"
          slidesToShow={4}
          center={false}
          title={'Ending Soon Competitions'}
        />
        {/* product section 2 */}
        <ProductSection
          class="max-w-md lg:max-w-sm xl:max-w-md ml-2   "
          slidesToShow={3}
          center={true}
          title="UPCOMING COMPETITIONS"
        />
        <CategorySection />
        
      </div>
      <HowtoSection />
      <WhyChoose />
      <Testimonials />
    </div>
  );
}

export default Home;
