import React from 'react';
import SelectWinnerDataTable from '~/components/common/table/eventCustomersTable';

function SelectWinnerTable() {
  return (
    <div className="justify-center items-center px-8 py-4">
      <div className="flex items-center justify-between mb-4">
        <div className=" text-4xl font-semibold">Product Customers</div>
      </div>
      <SelectWinnerDataTable />
    </div>
  );
}

export default SelectWinnerTable;
