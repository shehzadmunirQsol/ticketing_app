import { useEffect, useState } from 'react';
import { Button } from '~/components/ui/button';
import {
  Dialog,
  DialogContent,
  DialogDescription,
  DialogFooter,
  DialogHeader,
  DialogTitle,
} from '~/components/ui/dialog';
import { useToast } from '~/components/ui/use-toast';
import { trpc } from '~/utils/trpc';
import { LoadingDialog } from './loadingModal';
import { UpdateWinnerSchema, updateWinnerSchema } from '~/schema/winners';
import { useForm } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';
import { compressImage, isValidImageType } from '~/utils/helper';
import { getS3ImageUrl } from '~/service/api/s3Url.service';
import { Form } from '~/components/ui/form';
import { ImageInput } from '../file_input';

interface WinnersDialogInterface {
  isEnableModal: boolean;
  selectedItem: {
    name: string;
    winnerId: number;
    isEnable: boolean;
    thumb: string;
  };
  setSelectedItem: any;
  setIsEnableModal: any;
  refetch: any;
}

export function WinnersEnableDialog(props: WinnersDialogInterface) {
  const { toast } = useToast();
  const [loading, setLoading] = useState<boolean>(false);

  // Update CMS Status
  const updateWinner = trpc.winner.update.useMutation({
    onSuccess: (res: any) => {
      console.log(res);
      toast({
        variant: 'success',
        title: 'Winnar Updated Successfully',
      });
    },
    onError(error: any) {
      console.log(error);
    },
  });

  function handleClose() {
    props?.setIsEnableModal(false);
    props?.setSelectedItem({});
  }

  async function handleClick() {
    try {
      setLoading(true);
      const payload = {
        winner_id: props?.selectedItem?.winnerId,
        is_enabled: !props?.selectedItem?.isEnable,
      };
      const result = await updateWinner.mutateAsync(payload);

      if (result) {
        setLoading(false);
        handleClose();
        toast({
          variant: 'success',
          title: `Winner Enabled Successfully`,
        });

        props?.refetch();
      } else {
        throw new Error('Data update Error');
      }
    } catch (e: any) {
      setLoading(false);
      handleClose();
      toast({
        variant: 'destructive',
        title: e.message,
      });
    }
  }

  return (
    <>
      <Dialog open={props?.isEnableModal} onOpenChange={handleClose}>
        <DialogContent className="sm:max-w-[425px]">
          <DialogHeader>
            <DialogTitle className="text-left">
              {props?.selectedItem?.isEnable ? 'Disable' : 'Enable'} Winner
            </DialogTitle>
            <DialogDescription>
              <div className="flex flex-col gap-4">
                <div className="  flex gap-2 items-center p-2  ">
                  <p>
                    Are You Sure You Want to{' '}
                    {props?.selectedItem?.isEnable ? 'disable' : 'enable'}{' '}
                    <span className="text-primary">
                      {props?.selectedItem?.name}
                    </span>
                    ?
                  </p>
                </div>
              </div>
            </DialogDescription>
          </DialogHeader>
          <DialogFooter>
            <Button type="submit" onClick={() => handleClick()}>
              Yes
            </Button>
          </DialogFooter>
        </DialogContent>
      </Dialog>
      <LoadingDialog
        open={loading}
        text={
          props?.selectedItem?.isEnable
            ? 'Disabling Winnar...'
            : 'Enabling Winnar...'
        }
      />
    </>
  );
}

interface WinnersImageDialogInterface {
  isImageModal: boolean;
  selectedItem: {
    name: string;
    winnerId: number;
    isEnable: boolean;
    thumb: string;
  };
  setSelectedItem: any;
  setIsImageModal: any;
  refetch: any;
}

export function ImageUploadDialog(props: WinnersImageDialogInterface) {
  const { toast } = useToast();
  const [loading, setLoading] = useState<boolean>(false);
  const [optimizeFile, setOptimizeFile] = useState<any>(null);

  useEffect(() => {
    if (props?.selectedItem?.winnerId) {
      form.setValue('winner_id', props?.selectedItem?.winnerId);
      form.setValue('thumb', props?.selectedItem?.thumb);
    }
  }, [props?.selectedItem?.winnerId, props?.selectedItem?.thumb]);

  const form = useForm<UpdateWinnerSchema>({
    resolver: zodResolver(updateWinnerSchema),
    defaultValues: {
      winner_id: props?.selectedItem?.winnerId,
      thumb: props?.selectedItem?.thumb ?? '',
    },
  });

  const updateWinner = trpc.winner.update.useMutation({
    onSuccess: () => {
      console.log('upload successfully');
    },
    onError(error: any) {
      console.log({ error });
    },
  });

  async function singleImageHandler(originalFile: File) {
    const optimizedFile = await compressImage(originalFile);
    setOptimizeFile(optimizedFile);
  }

  async function uploadOnS3Handler(originalFile: any) {
    if (originalFile?.name) {
      const response = await getS3ImageUrl(originalFile);
      if (!response.success)
        return console.log('response.message', response.message);

      const isImage = isValidImageType(originalFile?.type);

      const nftSource = {
        thumb: '',
      };

      if (isImage) {
        nftSource.thumb = response?.data;
      }

      return nftSource;
    } else {
      return console.log('Please Select Image');
    }
  }

  async function onSubmit(values: UpdateWinnerSchema) {
    try {
      setLoading(true);

      if (values.thumb === '') {
        if (!optimizeFile) return alert('Please select an image');
        const nftSource = await uploadOnS3Handler(optimizeFile);
        values.thumb = nftSource ? nftSource?.thumb : '';
      }

      const payload = {
        winner_id: props?.selectedItem?.winnerId,
        thumb: values.thumb,
      };

      const data = await updateWinner.mutateAsync(payload);
      if (data) {
        setLoading(false);
        handleClose();

        toast({
          variant: `success`,
          title: `Uploaded Successfully`,
        });
        props?.refetch();
      }
    } catch (e: any) {
      setLoading(false);

      toast({
        variant: 'destructive',
        title: e.message,
      });
    }
  }

  function handleClose() {
    props?.setIsImageModal(false);
    props?.setSelectedItem({});
  }

  return (
    <Dialog open={props?.isImageModal} onOpenChange={handleClose}>
      <DialogContent className="sm:max-w-[750px]">
        <DialogHeader>
          <DialogTitle>Add Image</DialogTitle>
        </DialogHeader>
        <DialogDescription>
          <Form {...form}>
            <form onSubmit={form.handleSubmit(onSubmit)}>
              <ImageInput
                register={form.register('thumb')}
                reset={form.reset}
                getValues={form.getValues}
                setValue={form.setValue}
                onChange={singleImageHandler}
                onRemove={setOptimizeFile}
                required={true}
              />

              <div className="mt-4 flex items-center justify-end">
                <Button
                  type="submit"
                  variant={'clip'}
                  className="w-1/12 font-bold hover:opacity-80"
                >
                  Add
                </Button>
              </div>
            </form>
          </Form>
        </DialogDescription>

        <LoadingDialog open={loading} text={'Adding image...'} />
      </DialogContent>
    </Dialog>
  );
}
