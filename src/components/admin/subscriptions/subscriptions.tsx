import React from 'react';

import SubscriptionDataTable from '~/components/common/table/subscriptions';

function Subscription() {
  return (
    <div className=" px-8 py-4">
      <div className="flex items-center justify-between mb-4">
        <div className=" text-4xl font-semibold">Subscription</div>
      </div>
      <SubscriptionDataTable />
      {/* <OrderView /> */}
    </div>
  );
}

export default Subscription;
