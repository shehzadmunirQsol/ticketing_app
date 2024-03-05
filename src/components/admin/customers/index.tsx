import React from 'react';

import CustomersDataTable from '~/components/common/table/customers';
import RolesDataTable from '~/components/common/table/roles';
import { Tabs, TabsContent, TabsList, TabsTrigger } from '~/components/ui/tabs';

function Customers() {
  return (
    <div className="justify-center items-center px-8 py-4">
      <div className="flex items-center justify-between mb-4">
        <div className=" text-4xl font-semibold">Customers</div>
      </div>
      <div>
        <Tabs defaultValue="seller" className="w-full">
          <TabsList className=" px-2 w-1/2">
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
      </div>
    </div>
  );
}

export default Customers;
