import {
  Card,
  CardContent,
  CardDescription,
  CardHeader,
  CardTitle,
} from '@/ui/card';
import { Tabs, TabsContent } from '@/ui/tabs';
import { Overview } from './chart';
import { RecentSales } from './recent_sales';
import { trpc } from '~/utils/trpc';
import Link from 'next/link';

export default function DashboardPage() {
  return (
    <>
      <div className="flex-col md:flex">
        <div className="">
          <div className="flex h-6 items-center px-4"></div>
        </div>
        <div className="flex-1 space-y-2 px-2 lg:px-8 ">
          <div className="flex items-center justify-between space-y-2">
            <h2 className="text-3xl font-bold tracking-tight">Dashboard</h2>
            <div className="flex items-center space-x-2"></div>
          </div>
          <Tabs defaultValue="overview" className="space-y-4">
            <TabsContent value="overview" className="space-y-4">
              <AnalyticsCard />
              <div className="w-full grid gap-4 md:grid-cols-2 lg:grid-cols-7">
                <Card className="w-full lg:col-span-4 bg-transparent">
                  <CardHeader className="text-muted-foreground ">
                    <CardTitle>Sales Overview</CardTitle>
                  </CardHeader>
                  <CardContent className="pl-2 z-10">
                    <Overview />
                  </CardContent>
                </Card>
                <Card className="w-full lg:col-span-3">
                  <CardHeader className="text-muted-foreground ">
                    <CardTitle>Recent Sales</CardTitle>
                    <CardDescription></CardDescription>
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
  const { data, isLoading } = trpc.dashboard.analytics.useQuery(undefined, {
    refetchOnWindowFocus: false,
  });

  return (
    <div className="grid xss:grid-cols-1  text-white xs:grid-cols-1 sm:grid-cols-2 md:grid-cols-2 lg:grid-cols-12 gap-4 mt-4 justify-center align-middle ">
      {isLoading ? (
        <div className=" col-span-2 lg:col-span-4 items-center m-auto">
          <i className="fa-solid fa-circle-notch transition-all animate-spin text-lg  "></i>
        </div>
      ) : data?.data ? (
        data?.data.map((item: any, index: number) => {
          return (
            <Card
              key={index}
              className={`${
                item?.cols ? ' lg:col-span-6 ' : 'lg:col-span-4'
              } text-white hover:drop-shadow-md hover:shadow-primary  group cursor-pointer w-full border  rounded-lg shadow  `}
            >
              <Link href={item?.link}>
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
              </Link>
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
