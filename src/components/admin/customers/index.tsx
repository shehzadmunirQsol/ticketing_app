import { useRouter } from 'next/router';
import React, { useEffect, useState } from 'react';

import CustomersDataTable from '~/components/common/table/customers';
import { Tabs, TabsContent, TabsList, TabsTrigger } from '~/components/ui/tabs';

function Customers() {
  // const [tabState, setTabState] = useState('seller');
  const router = useRouter();
  // useEffect(() => {
  //   if (router.query?.type) {
  //     setTabState(router.query?.type as string);
  //   }
  // }, [router.query]);
  const tabState = React.useMemo(() => {
    return router.query?.type ? router.query?.type : 'seller';
  }, [router.query?.type]);
  return (
    <div className="justify-center items-center px-8 py-4">
      <div className="flex items-center justify-between mb-4">
        <div className=" text-4xl font-semibold">Customers</div>
      </div>
      <div>
        <CustomerTab tabState={tabState} />
      </div>
    </div>
  );
}
const CustomerTab = (props: any) => {
  return (
    <>
      <Tabs defaultValue={props?.tabState} className="w-full">
        <TabsList className=" px-2 w-full md:w-1/2">
          <TabsTrigger value="seller" className="w-full">
            Seller/Buyer
          </TabsTrigger>
          <TabsTrigger value="client" className="w-full">
            Client
          </TabsTrigger>
          <TabsTrigger value="trucker" className="w-full">
            Trucker
          </TabsTrigger>
        </TabsList>
        <TabsContent value="seller">
          <CustomersDataTable type={'seller'} />
        </TabsContent>
        <TabsContent value="client">
          <CustomersDataTable type={'client'} />
        </TabsContent>
        <TabsContent value="trucker">
          <CustomersDataTable type={'trucker'} />
        </TabsContent>
      </Tabs>
    </>
  );
};

export default Customers;
