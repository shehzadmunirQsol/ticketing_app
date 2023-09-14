import React from 'react';
import SelectWinnerDataTable from '~/components/common/table/selectWinnerTable';

function SelectWinnerTable() {
  return (
    <div className="justify-center items-center px-8 py-4">
      <div className="flex items-center justify-between mb-4">
        <div className=" text-4xl font-semibold">Event Customers</div>
      </div>
      <SelectWinnerDataTable />
    </div>
  );
}

export default SelectWinnerTable;
