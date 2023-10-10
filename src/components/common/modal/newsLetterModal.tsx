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
                    <ScrollArea className="w-full h-[calc(100vh-200px)] max-h-[580px]">
                        <ScrollBar orientation="vertical"></ScrollBar>

                        <iframe
                            width="100%"
                            height="580"
                            className=' p-0 w-full  rounded-lg !no-scrollbar '
                            scrolling="no"
                            src="https://bdc4c4ca.sibforms.com/serve/MUIFAMe5MNuD9wnTvTP0mJAr1lDtYWvnJ6Ah4JdDXQ1Vn-SFOimLyyPSrZL20moh8eqtTjLmCZXFWzHzHGdGMURNtzas4cQ5cmwx05yKqZVw8pdJMmZVHqJI1WZWocgzL8S1YrrSfKF2lQPNUna-K42L_N7cRy7bMw1LgOsTjGJkSVwAuFFX7QI-usxCc1Tfgq25JMbIjdRc-E1u"
                        ></iframe>
                    </ScrollArea>
                </DialogDescription>

            </DialogContent>
        </Dialog>
    );
}
