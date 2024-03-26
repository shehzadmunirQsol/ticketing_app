import { BellIcon, CheckIcon } from 'lucide-react';
import { useRouter } from 'next/router';
import React from 'react';
import { LoadingDialog } from '~/components/common/modal/loadingModal';
import CustomersDataTable from '~/components/common/table/customers';

import ProjectsDataTable from '~/components/common/table/projects';
import ProjectsTicketsDataTable from '~/components/common/table/projectsTickets';
import ProjectsTruckersDataTable from '~/components/common/table/projectsTruckers';
import { Badge } from '~/components/ui/Badge';
import { Button } from '~/components/ui/button';
import {
  Card,
  CardContent,
  CardDescription,
  CardFooter,
  CardHeader,
  CardTitle,
} from '~/components/ui/card';
import { Separator } from '~/components/ui/separator';
import { Switch } from '~/components/ui/switch';
import { Tabs, TabsContent, TabsList, TabsTrigger } from '~/components/ui/tabs';
import { displayDate } from '~/utils/helper';
import { trpc } from '~/utils/trpc';

function ProjectDetail() {
  const router = useRouter();
  const { index } = router.query;

  const {
    data: projectDetail,
    refetch,
    isLoading,
  } = trpc.project.getDetail.useQuery(
    {
      id: (index ? +index : 0) as number,
    },
    {
      refetchOnWindowFocus: false,
    },
  );

  const tabState = React.useMemo(() => {
    return router.query?.type ? router.query?.type : 'active';
  }, [router.query?.type]);
  return (
    <div className="justify-center items-center px-8 py-4 relative">
      <div className="grid grid-cols-12 gap-4 relative">
        <div className="col-span-12 sticky top-36  md:col-span-4 border-r-2 border-border  ">
          <div className="flex items-center justify-between py-2 px-2">
            <div className=" text-3xl font-semibold capitalize">
              {projectDetail?.data?.name ?? ''}
            </div>
            <div>
              <Badge
                variant={
                  projectDetail?.data?.is_invoiced ? 'default' : 'secondary'
                }
              >
                {projectDetail?.data?.is_invoiced ? 'Completed' : 'In-Progress'}
              </Badge>
            </div>
          </div>
          <Separator />
          <div className=" flex flex-col gap-2 mt-2 px-2">
            <ProjectDetailContent type="project" data={projectDetail?.data} />
            <ProjectDetailContent type="seller" data={projectDetail?.data} />
            <ProjectDetailContent type="client" data={projectDetail?.data} />
          </div>
        </div>

        <div className="w-full col-span-12 md:col-span-8 relative ">
          <ProjectTabs tabState={tabState} id={index ? +index : 0} />
        </div>
      </div>
      <LoadingDialog open={isLoading} text={'Loading data...'} />
    </div>
  );
}
const ProjectTabs = (props: any) => {
  return (
    <>
      <Tabs defaultValue={props?.tabState} className="w-full relative">
        <TabsList className=" px-2 w-full md:w-1/2 ml-auto  flex justify-end">
          <TabsTrigger value="active" className="w-full">
            Tickets
          </TabsTrigger>
          <TabsTrigger value="closed" className="w-full">
            Truckers
          </TabsTrigger>
        </TabsList>
        <Separator className="mt-1" />
        <TabsContent value="active">
          <ProjectsTicketsDataTable  />
        </TabsContent>
        <TabsContent value="closed">
          <ProjectsTruckersDataTable id={props?.id} />
        </TabsContent>
      </Tabs>
    </>
  );
};

const ProjectDetailContent = (props: any) => {
  const projectInfo = [
    {
      title: 'Project Name',
      description: props?.data?.name,
    },
    {
      title: 'Project Price',
      description: '$ ' + props?.data?.price,
    },
    {
      title: 'Total Rounds',
      description: props?.data?.total_rounds,
    },
    {
      title: 'Truck Capacity',
      description: props?.data?.truck_cap,
    },
    {
      title: 'Start Date',
      description: displayDate(props?.data?.start_date),
    },
    {
      title: 'Delivery Date',
      description: displayDate(props?.data?.delivery_date),
    },
  ];
  const clientInfo = [
    {
      title: 'Name',
      description: props?.data?.Client?.username,
    },
    {
      title: 'Email',
      description: props?.data?.Client?.email,
    },
    {
      title: 'Phone',
      description: props?.data?.Client?.phone_number,
    },
  ];
  const sellerInfo = [
    {
      title: 'Name',
      description: props?.data?.User?.username,
    },
    {
      title: 'Email',
      description: props?.data?.User?.email,
    },
    {
      title: 'Phone',
      description: props?.data?.User?.phone_number,
    },
  ];
  return (
    <>
      <Card>
        <CardHeader>
          <CardTitle className=" text-white capitalize">
            {props?.type}
          </CardTitle>
          {/* <CardDescription>You have 3 unread messages.</CardDescription> */}
        </CardHeader>
        <CardContent className="grid gap-4">
          <div>
            {(props?.type == 'project'
              ? projectInfo
              : props?.type == 'seller'
              ? sellerInfo
              : clientInfo
            ).map((item, index) => (
              <div
                key={index}
                className="mb-2 grid grid-cols-[25px_1fr] items-start pb-1 last:mb-0 last:pb-0"
              >
                <span className="flex h-2 w-2 translate-y-1 rounded-full bg-sky-500" />
                <div className="">
                  <p className="text-sm font-medium leading-none text-white flex justify-between items-center">
                    {item.title}
                    <span className="text-sm text-muted-foreground capitalize">
                      {' '}
                      {item.description}
                    </span>
                  </p>
                  {/* <p className="text-sm text-muted-foreground">
                      {item.description}
                    </p> */}
                </div>
              </div>
            ))}
          </div>
        </CardContent>
      </Card>
    </>
  );
};
export default ProjectDetail;
