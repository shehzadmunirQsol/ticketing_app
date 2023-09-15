// import { BiSolidChevronDown } from "react-s/bi";
import Image from 'next/image';

const Cms = ({cmsData}) => {
    console.log(cmsData,"cmsData")
  return (
<div>
<div
        dangerouslySetInnerHTML={{
          __html: cmsData?.CMSDescription[0]?.content?.toString() ?? '<>html conte</>',
        }}
      ></div>
</div>
  );
};

export default Cms;
