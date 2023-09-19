import React from 'react';
import WinnersDataTable from '~/components/common/table/winnersTable';

function Winners() {
  return (
    <div className="justify-center items-center px-8 py-4">
      <div className="flex items-center justify-between mb-4">
        <div className=" text-4xl font-semibold">Winners</div>
      </div>
      <WinnersDataTable />
    </div>
  );
}

export default Winners;
