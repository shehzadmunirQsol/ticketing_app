import React from 'react';

const VideoSection = (props: any) => {
  return (
    <div className="mt-10 mb-20">
      {props?.data?.video_src && (
        <iframe
          className="w-full h-[560px]"
          src={props?.data?.video_src}
          title=""
          // frameBorder="0"
          allow=" autoplay;"
          allowFullScreen
        ></iframe>
      )}
    </div>
  );
};

export default VideoSection;
