import { NextPageWithLayout } from '~/pages/_app';
import { Button } from '@/ui/button';
import { useTheme } from 'next-themes';
import Home from '~/components/app/home';
import { trpc } from '~/utils/trpc';

const IndexPage: NextPageWithLayout = () => {
  const { theme, setTheme } = useTheme();

  function toggleTheme() {
    const sysTheme: string = theme === 'dark' ? 'light' : 'dark';
    setTheme(sysTheme);
  }

  const registration = trpc.user.register.useMutation({
    onSuccess: (res: any) => {
      console.log("return data", res);
    },
    onError(error) {
      console.log( error.message,"ERROR" );
    },
  })

  async function register(){
    console.log("Working")
    const values={
      name:"umair",
      email:"umair.qsols@gmail.com",
      role_id:1,
      password:"umair12345"
    }
    console.log("values: ",values)
    try {
      const response = await registration.mutateAsync({ ...values });  
      console.log("Response : ",response)
    } catch (error : any ){
      console.log("Error ",error)
      // const errorMessage = formatTrpcError(error?.shape?.message);
      
    }
  }
  return (
    <div className="bg-background h-auto justify-center items-center  w-screen   ">
      <div className="">
        {/* <Button onClick={()=>register()}>Magic appears</Button> */}
        <Home />
      </div>
    </div>
  );
};

export default IndexPage;
