import React, { useState } from 'react';

const AddressesView = () => {
  const [isEdit, setEdit] = useState(true);
  return (
    <div className="py-4 px-6 text-[#eaeaea]">
      <p className="text-[#808080] text-sm">
        The following addresses will be used on the checkout page by default.
      </p>

      <div className="mt-4 flex ">
        <div className="w-64 h-52 rounded-lg border-[1px] p-4 border-[#808080] hover:border-primary flex flex-col">
          <div className="flex justify-between">
            <p className="text-md font-extrabold tracking-tight lead">
              Billing Address
            </p>
            <div className="bg-primary text-[#101417]">
              <p></p>Edit
              <span>
                <i className="fas fa-edit"></i>
              </span>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
};

export default AddressesView;
