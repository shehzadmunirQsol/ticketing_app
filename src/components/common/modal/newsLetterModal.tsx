import {
    Dialog,
    DialogContent,
    DialogDescription,
    DialogFooter,
    DialogHeader,
    DialogTitle,
} from '~/components/ui/dialog';

interface newsLetterDialogInterface {
    isModal: boolean;
    title?: string;
    setIsModal: any;
}
export function NewsLetterDialog(props: newsLetterDialogInterface) {
    return (
        <Dialog open={props?.isModal}  onOpenChange={(e) => props.setIsModal(e)}>
            <DialogContent className="p-0  ">
                
                <DialogDescription className='  '>
                    <iframe
                        
                        width="680"
                        className='block mx-auto max-w-full h-[610px] xsm:h-[530px]    rounded-lg overflow-hidden  no-scrollbar'
                        src="https://bdc4c4ca.sibforms.com/serve/MUIFAHS-vQxnOu9PaJF9s8xWBo4HeG9R_NidaLDkobSj6j8tQSZ9kghGAog8_UFBI_wzwiaMhAEbvimr8FXT2Ibl8Th_s6g2-8kqg9_iAmMd6CZox3d7mK5E2V-rV-ZuaFKJrFker58RP1CMmmfik-PvLu6XXXWRMAnAHqXkblcfw8Ogd1e5GG8yFfPEAkxgsc1WAlVBZhExO52e"
                        allowFullScreen
                    ></iframe>
                </DialogDescription>

            </DialogContent>
        </Dialog>
    );
}
