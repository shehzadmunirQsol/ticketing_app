import { Avatar, AvatarFallback, AvatarImage } from '@/ui/avatar';
import {
  customEmailTruncateHandler,
  customTruncateHandler,
} from '~/utils/helper';
import { trpc } from '~/utils/trpc';

export function RecentSales() {
  const { data: recentOrders, isLoading } = trpc.dashboard.recent.useQuery();
  console.log({ recentOrders });
  return (
    <div className="space-y-8">
      {isLoading ? (
        <div className=" col-span-2 lg:col-span-4 items-center m-auto">
          <i className="fa-solid fa-circle-notch transition-all animate-spin text-lg  "></i>
        </div>
      ) : recentOrders?.data ? (
        recentOrders?.data.map((item: any, index: number) => {
          return (
            <div key={index} className="flex items-center text-white">
              <Avatar className="h-9 w-9">
                <AvatarImage src="/avatars/05.png" alt="Avatar" />
                <AvatarFallback>SD</AvatarFallback>
              </Avatar>
              <div className="ml-4 space-y-1 ">
                <p className="text-sm font-medium leading-none">
                  {customTruncateHandler(
                    item?.Customer?.first_name +
                      ' ' +
                      item?.Customer?.last_name,
                  )}
                </p>
                <p className="text-sm text-muted-foreground">
                  {customEmailTruncateHandler(item?.Customer?.email, 10)}
                </p>
              </div>
              <div className="ml-auto font-medium">
                +AED {(item?.total_amount).toFixed(2)}
              </div>
            </div>
          );
        })
      ) : (
        <div className=" text-center col-span-2 lg:col-span-4">
          No Data Found!
        </div>
      )}
    </div>
  );
}
