import { useRouter } from 'next/router';
import React from 'react';

const Preview = () => {
  const router = useRouter();
  const { data } = router.query;
  console.log(data, 'HSJSGJSJHGUSJSHS');
  return (
    <div>
      {/* <p>hello</p> */}
      <div
        dangerouslySetInnerHTML={{ __html: data?.toString() ?? '<>html conte</>' }}
      ></div>
    </div>
  );
};

export default Preview;
