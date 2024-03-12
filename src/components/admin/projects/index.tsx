import { useRouter } from 'next/router';
import React from 'react';

import ProjectsDataTable from '~/components/common/table/projects';
import { Tabs, TabsContent, TabsList, TabsTrigger } from '~/components/ui/tabs';

function Projects() {
  const router = useRouter();

  const tabState = React.useMemo(() => {
    return router.query?.type ? router.query?.type : 'active';
  }, [router.query?.type]);
  return (
    <div className="justify-center items-center px-8 py-4">
      <div className="flex items-center justify-between mb-4">
        <div className=" text-4xl font-semibold">Projects</div>
      </div>
      <div>
        <ProjectTabs tabState={tabState} />
      </div>
    </div>
  );
}
const ProjectTabs = (props: any) => {
  return (
    <>
      <Tabs defaultValue={props?.tabState} className="w-full">
        <TabsList className=" px-2 w-full md:w-1/2">
          <TabsTrigger value="active" className="w-full">
            Active
          </TabsTrigger>
          <TabsTrigger value="closed" className="w-full">
            Closed
          </TabsTrigger>
        </TabsList>
        <TabsContent value="active">
          <ProjectsDataTable type={'active'} />
        </TabsContent>
        <TabsContent value="closed">
          <ProjectsDataTable type={'closed'} />
        </TabsContent>
      </Tabs>
    </>
  );
};

export default Projects;
