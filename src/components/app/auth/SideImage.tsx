import React from 'react';
import { Tabs, TabsContent, TabsList, TabsTrigger } from '~/components/ui/tabs';
import { Button } from '@/ui/button';
import {
  Form,
  FormControl,
  FormDescription,
  FormField,
  FormItem,
  FormLabel,
  FormMessage,
} from '@/ui/form';

import { Input } from '@/ui/input';
import { useForm } from 'react-hook-form';
import { useRouter } from 'next/router';
import { FileInput } from '~/components/common/file_input';
import { useEffect, useState } from 'react';
import { trpc } from '~/utils/trpc';
import { getS3ImageUrl } from '~/service/api/s3Url.service';
import { isValidImageType } from '~/utils/helper';
import { useToast } from '~/components/ui/use-toast';
import Image from 'next/image';
import CarImage from '../../../public/assets/CarLogin.svg';

const SideImage = () => {
  const form = useForm<any>();
  return (
    <div className='relative h-4/5'>
        <Image src={CarImage.src} width={51500} height={51500} alt="image" className='w-full h-full   object-contain' />
      <div className=' max-w-full h-full bg-white'>
      </div>
      <div className='absolute left-0 top-0 px-5 pt-10'>
        <p className='text-3xl w-72 '>Unlock Your Journey Login or Register for</p>
        <p className='font-black text-4xl uppercase mt-2'>Exclusive Access</p>
      </div>
    </div>
  );
};

export default SideImage;
