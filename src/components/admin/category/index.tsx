import React from 'react';
import DataTableBanner from '~/components/common/table/categoryTable';

function Category() {
  return (
    <div className="justify-center items-center px-8 py-4">
      <div className="flex items-center justify-between mb-4">
        <div className=" text-4xl font-semibold">Category</div>
      </div>
      <DataTableBanner />
    </div>
  );
}

export default Category;
