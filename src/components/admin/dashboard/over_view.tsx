import { Metadata } from 'next';
import Image from 'next/image';

import { Button } from '@/ui/button';
import {
  Card,
  CardContent,
  CardDescription,
  CardHeader,
  CardTitle,
} from '@/ui/card';
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/ui/tabs';
import { Overview } from './chart';
import { RecentSales } from './recent_sales';
import { trpc } from '~/utils/trpc';
// import { Overview } from '@/app/examples/dashboard/components/overview';
// import { RecentSales } from '@/app/examples/dashboard/components/recent-sales';

export const metadata: Metadata = {
  title: 'Dashboard',
  description: 'Example dashboard app built using the components.',
};

export default function DashboardPage() {
  return (
    <>
      <div className="flex-col md:flex">
        <div className="">
          <div className="flex h-6 items-center px-4">
            {/* <TeamSwitcher />
            <MainNav className="mx-6" /> */}
          </div>
        </div>
        <div className="flex-1 space-y-2 px-8 ">
          <div className="flex items-center justify-between space-y-2">
            <h2 className="text-3xl font-bold tracking-tight">Dashboard</h2>
            <div className="flex items-center space-x-2">
              {/* <CalendarDateRangePicker /> */}
              {/* <Button>Download</Button> */}
            </div>
          </div>
          <Tabs defaultValue="overview" className="space-y-4">
            {/* <TabsList>
              <TabsTrigger value="overview">Overview</TabsTrigger>
              
            </TabsList> */}
            <TabsContent value="overview" className="space-y-4">
              <AnalyticsCard />
              <div className="grid gap-4 md:grid-cols-2 lg:grid-cols-7">
                <Card className="col-span-4">
                  <CardHeader className="text-muted-foreground">
                    <CardTitle>Sales Overview</CardTitle>
                  </CardHeader>
                  <CardContent className="pl-2">
                    <Overview />
                  </CardContent>
                </Card>
                <Card className="col-span-3">
                  <CardHeader className="text-muted-foreground ">
                    <CardTitle>Recent Sales</CardTitle>
                    <CardDescription>
                      You made 265 sales this month.
                    </CardDescription>
                  </CardHeader>
                  <CardContent>
                    <RecentSales />
                  </CardContent>
                </Card>
              </div>
            </TabsContent>
          </Tabs>
        </div>
      </div>
    </>
  );
}
const AnalyticsCard = () => {
  const { data, isLoading } = trpc.dashboard.analytics.useQuery();
  
  return (
    <div className="grid gap-4 md:grid-cols-2 lg:grid-cols-4 ">
      {isLoading ? (
        <div className=" col-span-2 lg:col-span-4 items-center m-auto">
          <i className="fa-solid fa-circle-notch transition-all animate-spin text-lg  "></i>
        </div>
      ) : data?.data ? (
        data?.data.map((item: any, index: number) => {
          return (
            <Card key={index} className="text-white">
              <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
                <CardTitle className="text-sm text-white font-medium">
                  {item?.title}
                </CardTitle>
                <i
                  className={`${item?.icon} h-4 w-4 text-muted-foreground`}
                ></i>
              </CardHeader>
              <CardContent>
                <div className=" text-muted-foreground  font-bold">
                  <span className="text-2xl">{item?.data}</span>
                  <sub className="text-sm"> {item?.symbol}</sub>
                </div>
              </CardContent>
            </Card>
          );
        })
      ) : (
        <div className=" text-center col-span-2 lg:col-span-4">
          No Data Found!
        </div>
      )}
    </div>
  );
};
