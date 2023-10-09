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
        <Dialog open={props?.isModal} onOpenChange={(e) => props.setIsModal(e)}>
            <DialogContent className="p-0 ">
                
                <DialogDescription>
                    <iframe
                        width="680"
                        height="580"
                        src="https://bdc4c4ca.sibforms.com/serve/MUIFAP1b8OZMsKXfMNgzIinuyzvR4Hnxemz9U5LaApkI_TOp979ZGTlAyNQhDVyWDM_9IdobfL00fOvOVf3FUS4cKnYMLexustvnUP5qD4ojlTxeykRz0SZtb9ovTfBLhO6ifn-y0GrqyzXDb8E1yhuUOEISq5pIoJQjsrnaYNRCzfWScZhVj8RUQT6dhZPem1FN5mcx5msLEE2b"
                        frameBorder="0"
                        scrolling="auto"
                        allowFullScreen
                        style={{
                            display: 'block',
                            marginLeft: 'auto',
                            marginRight: 'auto',
                            maxWidth: '100%',

                        }}
                    ></iframe>
                </DialogDescription>

            </DialogContent>
        </Dialog>
    );
}
