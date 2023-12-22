'use client';
import React, { useEffect, useState } from 'react';
import BannerTitle from '~/components/common/banner_title';
import CashBg from '~/public/assets/Cash Prize Page Web.png';
import { useSelector } from 'react-redux';
import { RootState } from '~/store/store';
import ProductCard from '~/components/common/card';
import { trpc } from '~/utils/trpc';
import ProductSection from '../home/product_section';
import { setCookie, getCookie, deleteCookie } from '~/service/api/cookies.service';
// import Glow from '~/components/common/glow';

const CashPage = () => {
  const { lang } = useSelector((state: RootState) => state.layout);

  const { isLogin, user } = useSelector((state: RootState) => state.auth);
  const fullUrl =  typeof window !== 'undefined' ? window.location.href : "";

  const [products, setProducts] = useState<Array<any>>([]);
  const [mailtrigger, setMailtrigger] = useState(0);
  const eventFilters = {
    lang_id: lang?.lang_id,
    first: 0,
    rows: 9,
    category_id: 2,
  };
  const [filters, setFilters] = useState({
    ...eventFilters,
  });

  useEffect(() => {
    setFilters({ ...filters, lang_id: lang.lang_id, first: 0 });
    setProducts([]);
  }, [lang.lang_id]);

  const { data: prductsList } = trpc.event.getByCategoryId.useQuery(filters, {
    refetchOnWindowFocus: false,
    onSuccess(data) {
      if (filters.first > 0 && data?.data?.length) {
        setProducts([...products, ...data.data]);
      } else if (data?.data?.length) {
        setProducts(data.data);
      }
    },
  });

  function nextPage() {
    if (
      prductsList?.data?.length === filters.rows &&
      prductsList?.count > products.length
    ) {
      setFilters({ ...filters, first: ++filters.first });
    }
  }



  var cash1 = "";
  var cash2 = "";

  useEffect(() => {
    if(user?.email){ 

      if(products[0]?.EventDescription[0]){
        cash1 = products[0]?.EventDescription[0]?.name;
      }
      if(products[1]?.EventDescription[0]){
        cash2 = products[1]?.EventDescription[0]?.name;
      } 
      if(cash1 && cash2 && mailtrigger===0){ 
        setMailtrigger(mailtrigger+1);
        if ('sendinblue' in window && window?.sendinblue) {
          const sendinblue: any = window.sendinblue;
          
          var counterValue = getCookie('cashCounterValue');
          console.log("cashCounterValue",counterValue);
          if(counterValue){
            setCookie('cashCounterValue', parseInt(counterValue) + 1, 70);
          }else{
            setCookie('cashCounterValue', 1, 70);
          }

          if(counterValue && parseInt(counterValue)===2){
            sendinblue?.track(
              'page_visited',
              {
                "email": user.email,
                "FIRSTNAME": user.first_name
              },
              {
                "data": {
                  "car_name_1" : cash1,
                  "car_name_2": cash2,
                  "url": fullUrl
                }
              },
            ) as any;
          }


          //deleteCookie("cashCounterValue");
          
        }
      }
    }
  }, [user,products]);

  return (
    <>
      <div className="relative pt-24"></div>
      <BannerTitle image={CashBg} text={''} />
      <div className=" h-full px-4 space-y-8 py-14 md:px-14 md:space-y-12 md:py-20">
        <div className="relative">
          {/* Will look into it later when finalizing */}

          {/* <Glow className=" absolute  top-1/4 -right-16  p-2   w-1/6 h-[150px]  " />
          <Glow className=" absolute  bottom-14 -right-16  w-1/6 h-[150px] " /> */}
          <div className=" grid gap-6 grid-cols-1 sm:grid-cols-2  z-40  lg:grid-cols-3">
            {products?.map((itemList, i) => {
              return (
                <div className="z-40" key={i}>
                  <ProductCard
                    isLast={i === products.length - 1}
                    nextPage={nextPage}
                    dir={lang.dir}
                    data={itemList}
                  />
                </div>
              );
            })}
          </div>

          {/* doudt should it load more on action or automatically */}
          {products.length != prductsList?.count ? (
            <div className="w-fit mx-auto">
              <div className="text-center my-4">
                <p className="tracking-tight font-bold">Load More</p>
                <i className="fas fa-arrow-down  text-teal-400 text-5xl my-2  "></i>
              </div>
            </div>
          ) : (
            ''
          )}
        </div>

        <ProductSection
          class="w-3/5 md:w-full"
          slidesToShow={3}
          center={false}
          title={'Closed Competitions'}
          type="drawn"
          breakpoint={[3, 2, 1.5]}
          breakpointScreens={[1350, 1050, 800]}
          categoryId={2}
        />
      </div>
    </>
  );
};

export default CashPage;
