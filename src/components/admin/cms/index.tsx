import React from 'react';
import { Button } from '../../ui/button';
import Link from 'next/link';
import CouponsDataTable from '~/components/common/table/coupons';
import { trpc } from '~/utils/trpc';

function Cms() {
  const { data: cms, isLoading } = trpc.cms.getCmsContent.useQuery(
    {},
    {
      refetchOnWindowFocus: false,
    },
  );

  const handleEditCmsFunction = (id: number) => {
    console.log(id, 'handleEditCmsFunction working ');
  };

  console.log(cms, 'customer');
  return (
    <div className="justify-center items-center px-8 py-4">
      <div className="flex items-center justify-between mb-10">
        <div className=" text-4xl font-semibold">CMS Management</div>
        <Link href="/admin/cms/add">
          <Button type="submit" variant={'clip'} className="w-28">
            Add
          </Button>
        </Link>
      </div>
      <div className="py-4 px-4  w-full flex flex-col lg:flex-row md:flex-row items-center  flex-wrap  gap-4">
        {cms?.length ? (
          cms?.map((item: any, i) => {
            console.log(item, 'items');
            return (
              <div key={i} className="bg-background py-4 px-4 rounded-md xl:w-96 w-full shadow-lg flex flex-row items-center justify-between  ">
                <p className="text-2xl text-primary font-bold">
                  {item?.CMSDescription[0]?.title}
                </p>
                <div>
                  <div>
                    <Link href={`/admin/cms/edit/${item?.id}`}>
                      <Button
                        variant="outline"
                        size="icon"
                        className="xl:inline "
                      >
                        <i className="fa-solid fa-pen-to-square"></i>
                      </Button>
                    </Link>
                  </div>
                </div>
              </div>
            );
          })
        ) : (
          <div className="flex items-center justify-center w-full text-3xl">
            No Data Found
          </div>
        )}
      </div>
    </div>
  );
}

export default Cms;
