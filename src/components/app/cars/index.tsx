import React, { useEffect, useState } from 'react';
import { useSelector } from 'react-redux';
import ProductCard from '~/components/common/card';
import ProductListCard from '~/components/common/cardlistview';
import Glow from '~/components/common/glow';
import { RootState } from '~/store/store';
import { trpc } from '~/utils/trpc';
import FeaturedCars from './featured_cars';
import langContent from '~/locales';
import ProductSection from '../home/product_section';
import { setCookie, getCookie, deleteCookie } from '~/service/api/cookies.service';

const CarsPage = () => {
  const { lang } = useSelector((state: RootState) => state.layout);

  const { isLogin, user } = useSelector((state: RootState) => state.auth);
  const fullUrl =  typeof window !== 'undefined' ? window.location.href : "";

  const [cardView, setCardView] = useState<any>('cardview');

  const [products, setProducts] = useState<Array<any>>([]);
  const [mailtrigger, setMailtrigger] = useState(0);
  const eventFilters = {
    lang_id: lang?.lang_id,
    first: 0,
    rows: 9,
    category_id: 1,
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
  

  var car1 = "";
  var car2 = "";
  var carprize1 = "";
  var carprize2 = "";

  useEffect(() => {
    if(user?.email){ 

      if(products[0]?.EventDescription[0]){
        car1 = products[0]?.EventDescription[0]?.name;
        carprize1 = products[0]?.price;
      }
      if(products[1]?.EventDescription[0]){
        car2 = products[1]?.EventDescription[0]?.name;
        carprize2 = products[1]?.price;
      } 
      if(car1 && mailtrigger===0){ 
        setMailtrigger(mailtrigger+1);
        if ('sendinblue' in window && window?.sendinblue) {
          const sendinblue: any = window.sendinblue;
          
          var counterValue = getCookie('carCounterValue');
          console.log("carCounterValue",counterValue);
          if(counterValue){
            setCookie('carCounterValue', parseInt(counterValue) + 1, 70);
          }else{
            setCookie('carCounterValue', 1, 70);
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
                  "car_name_1" : car1,
                  "car_name_2": car2,
                  "url": fullUrl,
                  "ticket_prize": carprize1+" aed"
                }
              },
            ) as any;
          }




          //TEST

          // sendinblue?.track(
          //   'page_visited',
          //   {
          //     "email": user.email,
          //     "FIRSTNAME": user.first_name
          //   },
          //   {
          //     "data": {
          //       "car_name_1" : car1,
          //       "car_name_2": car2,
          //       "url": fullUrl,
          //       "ticket_prize": carprize1+" aed"
          //     }
          //   },
          // ) as any;
          

          //deleteCookie("carCounterValue");
          
        }
      }
    }
  }, [user,products]);


  return (
    <div className="mx-auto  w-full bg-background">
      {/* this div below ↓ it to add spacing to avoid header */}
      <div className="relative pt-24"></div>
      <FeaturedCars />
      <div className="h-full  space-y-8 py-8 md:py-14 px-4 md:px-14 md:space-y-12 md:py-20">
        <div className="relative">

          {
            products.length !== 0?
              <p className="hidden slg:block pb-6 text-2xl md:text-5xl tracking-tighter font-extrabold text-white ">
                {langContent[lang.lang].Cars.HEADING}
              </p>
            :
            null
          }
 

          <div className="block slg:hidden">
            <div className="flex justify-end listtabs mb-4">
              <span className={`tabbtn ${cardView === 'cardview' ? 'active' : ''}`} onClick={() => setCardView('cardview')}><i className="fa-regular fa-square-full"></i></span>
              <span className={`tabbtn ltr:ml-3 rtl:mr-3 ${cardView === 'listview' ? 'active' : ''}`} onClick={() => setCardView('listview')}><i className="fa-solid fa-align-justify"></i></span>
            </div>
          </div>

          <Glow className="absolute  top-1/2 -left-16 w-1/5 h-[350px] overflow-hidden " />
          <Glow className="absolute  bottom-0 -right-16 w-1/5 h-[350px] overflow-hidden " />


          {
            cardView === "cardview" ?
              <div className="grid gap-8 md:gap-6 grid-cols-1 sm:grid-cols-2 z-40 lg:grid-cols-3 justify-between mx-auto ">
                {/* {products?.map((itemList, i) => {
                  return (
                    <div className="z-40" key={itemList?.id}>
                      <ProductCard
                        isLast={i === products.length - 1}
                        nextPage={nextPage}
                        dir={lang.dir}
                        data={itemList}
                        class="z-50"
                      />
                    </div>
                  );
                })} */}

                {products
                  ?.sort((a, b) => {
                    const isLastDayA =
                      a?.end_date
                        ? a?.end_date?.getTime() <= Date.now() + 24 * 60 * 60 * 1000
                        : false;

                    const isLastDayB =
                      b?.end_date
                        ? b?.end_date?.getTime() <= Date.now() + 24 * 60 * 60 * 1000
                        : false;
                    return isLastDayB ? 1 : isLastDayA ? -1 : 0;
                  })
                  .map((itemList, i) => (
                    <div className="z-40" key={itemList?.id}>
                      <ProductCard
                        isLast={i === products.length - 1}
                        nextPage={nextPage}
                        dir={lang.dir}
                        data={itemList}
                        class="z-50"
                      />
                    </div>
                  ))}
              </div>
              :
              <div className="listviewbx grid gap-5 md:gap-6 grid-cols-1 sm:grid-cols-2 z-40 justify-between mx-auto">
                {products?.map((itemList, i) => {
                  return (
                    <div className="z-40" key={itemList?.id}>
                      <ProductListCard
                        isLast={i === products.length - 1}
                        nextPage={nextPage}
                        dir={lang.dir}
                        data={itemList}
                        class="z-50"
                      />
                    </div>
                  );
                })}
              </div>
          }


          {
            products.length !== 0?
              products.length != prductsList?.count ? (
                <div className="w-fit mx-auto">
                  <div className="text-center my-4">
                    <p className="tracking-tight font-bold">Load More</p>
                    <i className="fas fa-arrow-down  text-teal-400 text-5xl my-2  "></i>
                  </div>
                </div>
              ) : (
                ''
              )
            :
            null
            // <div className="w-fit mx-auto">
            //   <div className="text-center my-4">
            //     <p className="tracking-tight font-bold text-2xl">No ongoing raffles</p>
            //   </div>
            // </div>
          }
        </div>




        <ProductSection
          class="w-3/5 md:w-full"
          slidesToShow={3}
          center={false}
          title={langContent[lang.lang].Index.upcoming.HEADING}
          type="upcoming"
          categoryId={1}
          breakpoint={[3, 2, 1.5]}
          breakpointScreens={[1350, 1050, 800]}
        />


        <ProductSection
          class="w-3/5 md:w-full"
          slidesToShow={3}
          center={false}
          title={langContent[lang.lang].Index.closed.HEADING}
          type="drawn"
          breakpoint={[3, 2, 1.5]}
          breakpointScreens={[1350, 1050, 800]}
          categoryId={1}
        />
      </div>

      {/* <Testimonials /> */}
    </div>
  );
};

export default CarsPage;
