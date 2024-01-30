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
                            src="https://bdc4c4ca.sibforms.com/serve/MUIFAGBEegi0HlZt3IQV9DbGoI8k7xMP-BfEJ4Q-abFsDpfHHqwuT_RY7KI3azEZuho24cfWJKp1duMRkAmFAWL1DFnM8XTGNqguQN_w0Kzq5JSwpN3EHQ_qZHguEBkGqxSXl-Dz6kNjnACr58CEinQ58NRXWyBcIQWTBOGF4bD7ho5LKQN7m6MWU4Qubuv_N4IyUzr_LKZ3bT0F"
                        ></iframe>
                    </ScrollArea>
                </DialogDescription>

            </DialogContent>
        </Dialog>
    );
}
