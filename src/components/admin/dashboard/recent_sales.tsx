import { Avatar, AvatarFallback, AvatarImage } from '@/ui/avatar';
import {
  customEmailTruncateHandler,
  customTruncateHandler,
  displayDate,
} from '~/utils/helper';
import { trpc } from '~/utils/trpc';

export function RecentSales() {
  const { data: recentOrders, isLoading } = trpc.dashboard.recent.useQuery();
  console.log({ recentOrders });
  return (
    <div className="space-y-8">
      {isLoading ? (
        <div className="  items-center m-auto">
          <i className="fa-solid fa-circle-notch transition-all animate-spin text-lg  "></i>
        </div>
      ) : recentOrders?.data ? (
        recentOrders?.data.map((item: any, index: number) => {
          return (
            <div
              key={index}
              className="flex flex-row gap-2 jus w-full items-center text-white"
            >
              <Avatar className="h-9 w-9">
                <AvatarImage src="/avatars/05.png" alt="Avatar" />
                <AvatarFallback>SD</AvatarFallback>
              </Avatar>
              <div className="ml-4 space-y-1   ">
                <div className="flex items-center gap-4">
                  <p className="text-sm font-medium leading-none">
                    {customTruncateHandler(item?.name)}
                  </p>
                  <span className="text-xs rounded-md border w-fit px-2 text-muted-foreground">
                    {customTruncateHandler(item?.material_type)}
                  </span>
                </div>
                <p className="text-xs  w-fit text-muted-foreground">
                  Created At: {displayDate(item?.created_at)}
                </p>
                {/* {customEmailTruncateHandler(item?.User?.email, 10)} */}
              </div>
              <div className="ml-auto font-medium">
                +${(item?.price).toFixed(2)}
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
