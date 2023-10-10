import {
    Dialog,
    DialogContent,
    DialogDescription,
    DialogFooter,
    DialogHeader,
    DialogTitle,
} from '~/components/ui/dialog';
import { ScrollArea, ScrollBar } from '~/components/ui/scroll-area';

interface newsLetterDialogInterface {
    isModal: boolean;
    title?: string;
    setIsModal: any;
}
export function NewsLetterDialog(props: newsLetterDialogInterface) {
    return (
        <Dialog open={props?.isModal} onOpenChange={(e) => props.setIsModal(e)}>
            <DialogContent className="p-0">
                <DialogDescription className='p-0   '>
                    <ScrollArea className="w-full h-[calc(100vh-200px)] max-h-[580px] scroll-hide">
                        <ScrollBar orientation="vertical"></ScrollBar>

                        <iframe
                            width="100%"
                            height="580"
                            className=' p-0 w-full  rounded-lg !no-scrollbar '
                            scrolling="no"
                            src="https://bdc4c4ca.sibforms.com/serve/MUIFAGku8FXiREi3fiNZGNeONgr0JM4YQYbK0JobEZDw-k80OJK6gA69wBS_OPzA8HR4C_x064hwo9iRzWUWxfVOg7OQdiQzjMUocJ8lOB5u3FG8qs_jQhnkXV2aEFrqkhIFD5kzOeFMrGlFa0vff2hqtpEToUmfMQ-4EPDs50FRcWhe6qJfYTzswRMfMLQ3sCL26BUQ1Nch6ETa"
                        ></iframe>
                    </ScrollArea>
                </DialogDescription>

            </DialogContent>
        </Dialog>
    );
}
