import React from 'react';

import OrdersDataTable from '~/components/common/table/orders';
import OrderView from './view';

function Orders() {
  return (
    <div className=" px-8 py-4">
      <div className="flex items-center justify-between mb-4">
        <div className=" text-4xl font-semibold">Orders</div>
      </div>
      <OrdersDataTable />
      {/* <OrderView /> */}
    </div>
  );
}

export default Orders;
