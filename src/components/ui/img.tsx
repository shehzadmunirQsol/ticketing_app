import Image, { ImageProps } from 'next/image';
import React from 'react';

export default function NextImage(props: ImageProps) {
  return (
    <Image
      {...props}
      onLoadingComplete={(image) => image?.classList?.remove('animate-pulse')}
      className={` ${props?.className} bg-text-light-secondary transition-all duration-2000 animate-pulse`}
    />
  );
}
