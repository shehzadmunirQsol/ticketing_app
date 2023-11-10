import React from 'react';
import LogoThumb from '~/public/assets/logo_video.png';
import Play from '~/public/assets/play.png';
import Share from '~/public/assets/icons/send.svg';
import { Dialog, DialogContent, DialogTrigger } from '@/ui/dialog';
import { renderNFTImage } from '~/utils/helper';
import NextImage from '../ui/img';

interface CardInterface {
  class?: string;
  dir?: string;
  data?: any;
}
const VideoCard = (props: CardInterface) => {
  return (
    <div className={`relative group  ${props?.class} `}>
      <Dialog>
        <div
          className={`relative mr-4   h-[400px]  group-hover:cursor-pointer  rounded-sm   shadow-lg bg-card flex justify-between items-start`}
        >
          <div className="absolute flex mt-2  justify-between items-center w-full  z-30">
            <div className="relative h-10 w-10  rounded-full bg-black z-40 ml-2">
              <NextImage src={LogoThumb} alt="/" fill />
            </div>

            {/* <i className="fa fa-share-from-square text-white text-2xl "></i> */}
            <DialogTrigger>
              <div
                className=" z-50 mr-2"
                onClick={() => {
                  console.log('in sharing');
                }}
              >
                <NextImage src={Share} alt="/" width={30} height={30} />
              </div>
            </DialogTrigger>
          </div>
          <NextImage
            src={renderNFTImage(props?.data)}
            fill
            alt=""
            className="absolute object-cover group-hover:opacity-60"
          />
          <DialogTrigger>
            <div className="absolute h-full w-full flex justify-center mx-auto  items-center ">
              {/* <i className="fa-regular fa-circle-play text-6xl  "></i> */}
              <NextImage src={Play} alt="/" className="w-14 h-14" />
            </div>
          </DialogTrigger>
          <DialogContent>
            <iframe
              className="w-full h-[360px] p-2"
              // src="https://www.youtube.com/embed/Y-x0efG1seA" //add src prop later
              src={`${props?.data?.link}`} //add src prop later
              title="YouTube video player"
              // frameBorder="0"
              allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share"
              allowFullScreen
            ></iframe>
          </DialogContent>
        </div>
      </Dialog>
    </div>
  );
};

export default VideoCard;
