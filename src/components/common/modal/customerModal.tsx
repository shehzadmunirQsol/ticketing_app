import { zodResolver } from '@hookform/resolvers/zod';
import { useForm } from 'react-hook-form';
import { Button } from '~/components/ui/button';
import {
  Dialog,
  DialogContent,
  DialogDescription,
  DialogHeader,
  DialogTitle,
} from '~/components/ui/dialog';
import {
  Form,
  FormControl,
  FormField,
  FormItem,
  FormLabel,
  FormMessage,
} from '~/components/ui/form';
import { Input } from '~/components/ui/input';

import { useToast } from '~/components/ui/use-toast';

import { trpc } from '~/utils/trpc';
import { LoadingDialog } from './loadingModal';
import { formatTrpcError } from '~/utils/helper';
import { useEffect } from 'react';

import 'react-international-phone/style.css';
import { addResourceInput, addResourcesSchema } from '~/schema/roles';

interface CustomerInterface {
  isModal: boolean;
  setIsModal: (data: boolean) => void;
  refetch: () => void;
  type: string;
  id?: number;
  name?: string | null;
  code?: string | null;
}

export function CustomerUploadDialog(props: CustomerInterface) {
  const { toast } = useToast();

  useEffect(() => {
    form.setValue('name', props.name ?? '');
    form.setValue('code', props.code ?? '');
  }, [props.isModal]);

  const addResource = trpc.roles.uploadResources.useMutation({
    onSuccess: (res) => {
      console.log(res);
      toast({
        variant: 'success',
        title: 'Address added successfully',
      });
    },
  });

  const updateResource = trpc.roles.uploadResources.useMutation({
    onSuccess: (res) => {
      console.log(res);
      toast({
        variant: 'success',
        title: 'Address Updated Successfully',
      });
    },
    onError(error) {
      const errorMessage = formatTrpcError(error?.shape?.message);

      toast({
        variant: 'destructive',
        title: errorMessage,
      });
    },
  });

  const form = useForm<addResourceInput>({
    resolver: zodResolver(addResourcesSchema),
    defaultValues: {
      name: props.name ?? '',
      code: props.code ?? '',
    },
  });

  async function onSubmit(values: addResourceInput) {
    try {
      const payload = { ...values };
      if (props?.id) {
        await updateResource.mutateAsync({ ...payload, id: props?.id });
      } else {
        await addResource.mutateAsync(payload);
      }

      toast({
        variant: 'success',
        title: `Address ${props?.id ? 'Update' : 'Add'} successfully!`,
      });
      props?.refetch();
      props?.setIsModal(false);
    } catch (error: any) {
      props?.setIsModal(false);
      toast({
        variant: 'destructive',
        title: error?.message ?? 'Something went wrong!',
      });
    }
  }
  useEffect(() => {
    const title = form?.watch('code');
    const altText = title
      ?.toLowerCase()
      .replaceAll(' ', '.')
      ?.replace(/[^a-zA-Z0-9._-]/g, '')
      .toLowerCase();
    form.setValue('code', altText);
  }, [form?.watch('code')]);

  console.log({ props });

  return (
    <Dialog open={props?.isModal} onOpenChange={() => props.setIsModal(false)}>
      <DialogContent className="max-h-[40vh] h-[calc(100%-200px)] max-w-xl md:max-w-[500px] overflow-y-hidden scroll-hide py-4 px-6">
        <DialogHeader>
          <DialogTitle className=" capitalize">
            {props?.id ? 'Update' : 'Add'} {props?.type}
          </DialogTitle>
          <DialogDescription>
            <Form {...form}>
              <form
                onSubmit={form.handleSubmit(onSubmit)}
                className="justify-center items-center  py-4"
              >
                <FormField
                  control={form.control}
                  name="name"
                  render={() => (
                    <FormItem className="w-full">
                      <FormLabel className="text-xs  font-thin text-cardGray ">
                        Name
                      </FormLabel>
                      <FormControl>
                        <Input
                          type="text"
                          className="rounded-md"
                          {...form.register('name')}
                        />
                      </FormControl>
                      <div className="relative pb-2 errormsg">
                        <FormMessage />
                      </div>
                    </FormItem>
                  )}
                />
                <FormField
                  control={form.control}
                  name="code"
                  render={() => (
                    <FormItem className="w-full">
                      <FormLabel className="text-xs  font-thin text-cardGray ">
                        Email
                      </FormLabel>
                      <FormControl>
                        <Input
                          type="text"
                          className="rounded-md"
                          {...form.register('code')}
                        />
                      </FormControl>
                      <div className="relative pb-2 errormsg">
                        <FormMessage />
                      </div>
                    </FormItem>
                  )}
                />

                <div className="flex items-center justify-end gap-4 mt-4">
                  <Button
                    variant="secondary"
                    type="button"
                    disabled={addResource.isLoading || updateResource.isLoading}
                    onClick={() => props.setIsModal(false)}
                  >
                    Cancel
                  </Button>
                  <Button
                    type="submit"
                    disabled={addResource.isLoading || updateResource.isLoading}
                  >
                    {props?.id ? 'Update' : 'Add'}
                  </Button>
                </div>
              </form>
            </Form>
          </DialogDescription>
        </DialogHeader>
      </DialogContent>
      <LoadingDialog
        open={addResource.isLoading || updateResource.isLoading}
        text={`${props?.id ? 'Updating' : 'Adding'} Resource...`}
      />
    </Dialog>
  );
}
