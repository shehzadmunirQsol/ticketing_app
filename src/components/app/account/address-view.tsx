import React, { useState } from 'react';


const AddressesView = () => {
  const [isEdit, setEdit] = useState(false);
  const handleChange = () => setEdit(!isEdit);

  const data1 = undefined;
  const data = {
    name: 'Hassan Shan',
    pobox: '9966',
    street: 'Al Qouz',
    number: '+971 50 446 2752',
    city: 'Dubai', //dropdown
  };
  return (
    <div className="py-4 px-6 text-[#eaeaea]">
      <p className="text-[#808080] text-sm">
        The following addresses will be used on the checkout page by default.
      </p>

      <div className="mt-4 flex ">
        <div
          className={`w-64 h-48 rounded-md border-[1px] p-4 ${
            data ? 'border-primary' : 'border-[#808080]'
          }  flex flex-col`}
        >
          <div className="flex justify-between items-center">
            <p className="text-md font-bold tracking-tight lead">
              Billing Address
            </p>
            <div className="px-2 py-1.5 bg-primary text-sm text-[#101417] font-extrabold tracking-tight leading-tight w-fit rounded-full flex justify-center gap-1 hover:cursor-pointer hover:opacity-90">
              <span>
                <i className={`fas ${data ? 'fa-pencil' : 'fa-plus'} `}></i>
              </span>
              <p className="text-xs">{data ? 'EDIT' : 'ADD'}</p>
            </div>
          </div>

          <div className="w-full h-full font-light text-[#eaeaea] flex flex-col pt-3 text-sm">
            {data == undefined && (
              <p className=" ">You have not set up this type of address yet.</p>
            )}
            {data ? (
              <div className="h-fit ">
                <input
                  className="bg-primary-foreground p-1"
                  value={data.name}
                  type="text"
                />
                <input
                  className="bg-primary-foreground p-1"
                  value={`P.O Box ${data.pobox}`}
                  type="text"
                />
                <input
                  className="bg-primary-foreground p-1"
                  value={data.street}
                  type="text"
                />
                <input
                  className="bg-primary-foreground p-1"
                  value={data.number}
                  type="tel"
                />
                <input
                  className="bg-primary-foreground p-1"
                  value={data.city}
                  type="text"
                />
              </div>
            ) : (
              ''
            )}
          </div>
        </div>
      </div>
    </div>
  );
};

export default AddressesView;
