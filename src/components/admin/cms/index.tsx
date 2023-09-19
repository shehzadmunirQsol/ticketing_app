import React, { useEffect } from 'react';
import { Button } from '../../ui/button';
import Link from 'next/link';
import CouponsDataTable from '~/components/common/table/coupons';
import { trpc } from '~/utils/trpc';
import {
  DropdownMenu,
  DropdownMenuCheckboxItem,
  DropdownMenuContent,
  DropdownMenuItem,
  DropdownMenuLabel,
  DropdownMenuSeparator,
  DropdownMenuTrigger,
} from '@/ui/dropdown-menu';
import { MoreHorizontal } from 'lucide-react';
import { useToast } from '~/components/ui/use-toast';
import { LoadingDialog } from '~/components/common/modal/loadingModal';
import { CmsDailog } from '~/components/common/modal/cms';

function Cms() {
  const { toast } = useToast();
  const [selectedItem, setSelectedItem] = React.useState({});
  const [title, setTitle] = React.useState('');
  const [type, setType] = React.useState('');
  const [isModal, setIsModal] = React.useState(false);

  const {
    data: cms,
    refetch,
    isLoading,
  } = trpc.cms.getCmsContent.useQuery(
    {},
    {
      refetchOnWindowFocus: false,
    },
  );

  // Initialize arrays to store data by type
  const staticData: any = [];
  const eventfaqsData: any = [];

  // Iterate through the original array and distribute items by type
  cms?.forEach((item) => {
    if (item.type === 'static') {
      staticData.push(item);
    } else if (item.type === 'event_faqs') {
      eventfaqsData.push(item);
    }
  });

  const handleCmsStatus = async (id: any, type: any) => {
    setSelectedItem(id);
    setTitle('CMS');
    setType(type);
    setIsModal(true);
  };

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
      <div className="py-4 px-4  w-full flex flex-col lg:flex-row md:flex-row items-center space-y-4 flex-wrap  gap-4">
        <div className="w-[calc(100vw-20%)]">
          <p className=" text-4xl font-semibold mb-10">Static CMS</p>

          <div className="grid grid-cols-1 gap-4 md:grid-cols-2 lg:grid-cols-2 xl:grid-cols-2 2xl:grid-cols-3  mb-10 ">
            {staticData?.length ? (
              staticData?.map((item: any, i: any) => {
                console.log(item, 'items');
                return (
                  <div
                    key={i}
                    className="bg-background py-4 px-4 rounded-md xl:w-96 w-full  mb-10 shadow-lg flex flex-row items-center justify-between  "
                  >
                    <p className="text-2xl text-primary font-bold">
                      {item?.CMSDescription[0]?.title}
                    </p>
                    <div>
                      <div>
                        <DropdownMenu>
                          <DropdownMenuTrigger asChild>
                            <Button variant="ghost" className="h-8 w-8 p-0">
                              <span className="sr-only">Open menu</span>
                              <MoreHorizontal className="h-4 w-4" />
                            </Button>
                          </DropdownMenuTrigger>
                          <DropdownMenuContent align="end">
                            <Link href={`/admin/cms/edit/${item?.id}`}>
                              <DropdownMenuItem>Edit CMS</DropdownMenuItem>
                            </Link>
                            <DropdownMenuSeparator />
                            <DropdownMenuItem
                              onClick={() =>
                                handleCmsStatus(
                                  item?.id,
                                  item?.is_enabled === false
                                    ? 'enabled'
                                    : 'disabled',
                                )
                              }
                            >
                              {item.is_enabled === false
                                ? 'Enabled'
                                : 'Disabled'}
                            </DropdownMenuItem>
                          </DropdownMenuContent>
                        </DropdownMenu>
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

        <div className="w-[calc(100vw-20%)]">
          <p className=" text-4xl font-semibold mb-10">Events CMS</p>

          <div className="grid grid-cols-1 gap-4 md:grid-cols-2 lg:grid-cols-2 xl:grid-cols-2 2xl:grid-cols-3  mb-10 ">
            {eventfaqsData?.length ? (
              eventfaqsData?.map((item: any, i: any) => {
                console.log(item, 'items');
                return (
                  <div
                    key={i}
                    className="bg-background py-4 px-4 rounded-md xl:w-96 w-full  mb-10 shadow-lg flex flex-row items-center justify-between  "
                  >
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
      </div>
      <CmsDailog
        selectedItem={selectedItem}
        setSelectedItem={setSelectedItem}
        title={title}
        setTitle={setTitle}
        isModal={isModal}
        setIsModal={setIsModal}
        refetch={refetch}
        type={type}
        setType={setType}
      />
      <LoadingDialog open={isLoading} text={'Loading data...'} />
    </div>
  );
}

export default Cms;
