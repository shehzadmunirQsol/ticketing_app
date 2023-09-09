import React from 'react';
import ImageSlider from './ImageSlider';
import EntiresDetail from './EntiresDetail';
import VideoCard from '~/components/common/video_card';

const VideoSection = () => {
  return (
    <div className="mt-10 mb-20">
      <iframe
        className="w-full h-[560px]"
        src="https://www.youtube.com/embed/Y-x0efG1seA?controls=0"
        title=""
        // frameBorder="0"
        allow=" autoplay;"
        allowFullScreen
      ></iframe>    
    </div>
  );
};

export default VideoSection;
