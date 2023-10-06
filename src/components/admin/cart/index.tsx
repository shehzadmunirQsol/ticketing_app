import React from 'react';
import CartDataTable from '~/components/common/table/cartTable';

function CartTable() {
  return (
    <div className=" px-8 py-4">
      <div className="flex items-center justify-between mb-4">
        <div className=" text-4xl font-semibold">Abandoned Cart</div>
      </div>
      <CartDataTable />
    </div>
  );
}

export default CartTable;
