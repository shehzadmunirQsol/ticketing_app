import Image from 'next/image';
import React from 'react';
import VideoThumb from '~/public/assets/video_thumb.png';
import LogoThumb from '~/public/assets/logo_video.png';
import Play from '~/public/assets/play.png';

import {
  Dialog,
  DialogContent,
  DialogDescription,
  DialogHeader,
  DialogTitle,
  DialogTrigger,
} from '@/ui/dialog';

interface cardInterface {
  class?: string;
  dir?: string;
}
const VideoCard = (props: cardInterface) => {
  const openModal = () => {
    console.log('it me');
  };

  return (
    <div className="relative group">
      <Dialog >
        <div
          onClick={openModal}
          className={` relative w-[300px] h-[400px] group-hover:cursor-pointer  rounded-sm overflow-hidden  shadow-lg bg-card flex justify-between items-start  ${props?.class}`}
        >
          <div className="absolute flex mt-2  justify-between items-center w-full  z-30">
            <div className="relative h-10 w-10  rounded-full bg-black z-40 ml-2">
              <Image src={LogoThumb} alt="/" fill />
            </div>

            <div className=" z-40">
              <i className="fa fa-share-from-square text-white text-2xl mr-2"></i>
            </div>
          </div>
          <Image
            src={VideoThumb}
            fill
            alt=""
            className="absolute object-cover group-hover:opacity-5"
          />
          <DialogTrigger>
            <div className="absolute h-full w-full flex justify-center mx-auto  items-center ">
              {/* <i className="fa-regular fa-circle-play text-6xl  "></i> */}
              <Image src={Play} alt="/" className="w-14 h-14" />
            </div>
          </DialogTrigger>
        </div>

        <DialogContent>
          <iframe
            className='w-full h-[360px]'
            src="https://www.youtube.com/embed/Y-x0efG1seA"
            title="YouTube video player"
            // frameBorder="0"
            allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share"
            allowFullScreen
          ></iframe>
        </DialogContent>
      </Dialog>
    </div>
  );
};

export default VideoCard;
